{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Cloud.AWS.Lib.Parser.Unordered
    ( SimpleXML
    , ParseError (..)
    , xmlParser
    , xmlParserM
    , xmlParserConduit
    , getT, (.<)
    , getElementM
    , getElement
    , getElements
    , content
    ) where

import Cloud.AWS.Lib.FromText (FromText (..))
import Control.Applicative ((<$>))
import Control.Exception (Exception)
import Control.Monad (liftM)
import Control.Monad.Trans (lift)
import Data.Char (isSpace)
import Data.Conduit (ConduitM, Conduit, yield, MonadThrow (..))
import qualified Data.Conduit.List as CL
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.XML.Types (Event (..), Name (..), Content (..))
import Text.XML.Stream.Parse (XmlException (..))

data SimpleXML = Map (HashMap Text [SimpleXML])
               | Content Text
               deriving (Show)

data ParseError = ParseError
    { parseErrorMessage :: Text
    } deriving (Show, Typeable)
instance Exception ParseError

xmlParser :: MonadThrow m
          => (SimpleXML -> m a)
          -> ConduitM Event o m a
xmlParser parse = xmlParserM parse >>=
    maybe (lift $ monadThrow $ ParseError "xmlParser: invalid xml") return

xmlParserM :: MonadThrow m
           => (SimpleXML -> m a)
           -> ConduitM Event o m (Maybe a)
xmlParserM parse = do
    xmlm <- getXML
    case xmlm of
        Just xml -> lift $ liftM Just $ parse xml
        Nothing -> return Nothing

xmlParserConduit :: MonadThrow m
                 => Text -- ^ name of item set
                 -> (SimpleXML -> m a) -- ^ item parser
                 -> Conduit Event m a
xmlParserConduit set parse = do
    e <- dropWS
    case e of
        Just (EventBeginElement name _) | nameLocalName name == set -> do
            CL.drop 1
            innerParser parse
        _ -> monadThrow $ ParseError $ "xmlParserConduit: no element '" <> set <> "'"
  where
    innerParser parse' = do
        ma <- xmlParserM parse'
        case ma of
            Just a -> yield a >> innerParser parse'
            Nothing -> return ()

getXML :: MonadThrow m
       => ConduitM Event o m (Maybe SimpleXML)
getXML = do
    e <- dropWS
    case e of
        Just (EventBeginElement name _) -> do
            CL.drop 1
            xmls <- getXMLList
            let xml = Map $ HM.singleton (nameLocalName name) $ case xmls of
                    [Content _] -> xmls
                    _ -> [Map $ foldr (HM.unionWith (++) . toHMap) HM.empty xmls]
            e' <- dropWS
            case e' of
                Just (EventEndElement name')
                    | name == name' -> CL.drop 1 >> return (Just xml)
                _ -> lift $ monadThrow $ XmlException ("Expected end tag: " ++ show name) e'
        Just (EventContent (ContentText t)) -> CL.drop 1 >> return (Just $ Content t)
        _ -> return Nothing
  where
    getXMLList = do
        e <- dropWS
        case e of
            Just EventEndElement{} -> return []
            _ -> do
                xml <- getXML
                case xml of
                    Just xml' -> (xml' :) <$> getXMLList
                    Nothing -> return []
    toHMap (Map hmap) = hmap
    toHMap _ = error "toHMap: invalid structure"

dropWS :: Monad m => ConduitM Event o m (Maybe Event)
dropWS = do -- drop white space
    e <- CL.peek
    if isWS e then CL.drop 1 >> dropWS else return e
  where
    isWS e = case e of -- is white space
        Just EventBeginDocument -> True
        Just EventEndDocument -> True
        Just EventBeginDoctype{} -> True
        Just EventEndDoctype -> True
        Just EventInstruction{} -> True
        Just EventBeginElement{} -> False
        Just EventEndElement{} -> False
        Just (EventContent (ContentText t))
            | T.all isSpace t -> True
            | otherwise -> False
        Just (EventContent ContentEntity{}) -> False
        Just EventComment{} -> True
        Just EventCDATA{} -> False
        Nothing -> False

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM a Nothing  = a
fromMaybeM _ (Just a) = return a

getContentText :: SimpleXML -> Maybe Text
getContentText (Map _) = Nothing
getContentText (Content c) = return c

getSubXMLs :: SimpleXML -> Text -> [SimpleXML]
getSubXMLs (Map hmap) name = cat $ HM.lookup name hmap
  where
    cat (Just xs) = xs
    cat Nothing = []
getSubXMLs (Content _) _ = []

getSubXMLM :: SimpleXML -> Text -> Maybe SimpleXML
getSubXMLM xml name = listToMaybe $ getSubXMLs xml name

getT :: (MonadThrow m, FromText a) => SimpleXML -> Text -> m a
getT xml name = fromNamedText name $
    getSubXMLM xml name >>= getContentText

-- | infix version of getT. like aeson's (.:).
(.<) :: (MonadThrow m, FromText a) => SimpleXML -> Text -> m a
(.<) = getT

getElementM :: MonadThrow m => SimpleXML -> Text -> (SimpleXML -> m a) -> m (Maybe a)
getElementM xml name parse = case getSubXMLM xml name of
    Just xml' -> liftM Just $ parse xml'
    Nothing -> return Nothing

getElement :: MonadThrow m => SimpleXML -> Text -> (SimpleXML -> m a) -> m a
getElement xml name parse = getElementM xml name parse >>=
    fromMaybeM (monadThrow $ ParseError $ "getElement: element '" <> name <> "' not found")

getElements :: MonadThrow m => SimpleXML -> Text -> Text -> (SimpleXML -> m a) -> m [a]
getElements xml set item parse = case getSubXMLM xml set of
    Just xml' -> mapM parse $ getSubXMLs xml' item
    Nothing -> return []

content :: MonadThrow m => SimpleXML -> m Text
content (Content t) = return t
content _           = fail "This is not a content."
