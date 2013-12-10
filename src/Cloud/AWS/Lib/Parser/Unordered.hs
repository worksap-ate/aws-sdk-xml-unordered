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
               deriving (Show, Eq)

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
    xmlm <- getMultiXML
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
    innerParser :: MonadThrow m => (SimpleXML -> m a) -> Conduit Event m a
    innerParser parse' = do
        xmlm <- getSingleXML
        case xmlm of
            Just xml -> do
                a <- lift $ parse' xml
                yield a
                innerParser parse'
            Nothing -> return ()

getSingleXML :: MonadThrow m
             => ConduitM Event o m (Maybe SimpleXML)
getSingleXML = do
    e <- dropWS
    case e of
        Just (EventBeginElement name _) -> do
            CL.drop 1
            xmls <- getXMLList
            let xml = Map $ HM.singleton (nameLocalName name) $ foldXML xmls
            e' <- dropWS
            case e' of
                Just (EventEndElement name')
                    | name == name' -> CL.drop 1 >> return (Just xml)
                _ -> lift $ monadThrow $ XmlException ("getSingleXML: Expected end tag for: " ++ show name) e'
        Just (EventContent (ContentText t)) -> CL.drop 1 >> return (Just $ Content t)
        _ -> return Nothing

foldXML :: [SimpleXML] -> [SimpleXML]
foldXML [] = []
foldXML xmls@(Content _ : _) = [Content $ T.concat . map toContent $ xmls]
  where
    toContent (Map _) = error $ "getSingleXML: Unexpected structure. Please report. " ++ show xmls
    toContent (Content t) = t
foldXML xmls@(Map _ : _) = case [Map $ foldr (HM.unionWith (++) . toHMap) HM.empty xmls] of
    [Map hmap] | hmap == HM.fromList [] -> []
    xmls' -> xmls'
  where
    toHMap (Map hmap) = hmap
    toHMap _ = error $ "getSingleXML: Unexpected structure. Please report. " ++ show xmls

getMultiXML :: MonadThrow m
            => ConduitM Event o m (Maybe SimpleXML)
getMultiXML = do
    xmls <- getXMLList
    return $ listToMaybe $ foldXML xmls

getXMLList :: MonadThrow m
           => ConduitM Event o m [SimpleXML]
getXMLList = do
    e <- dropWS
    case e of
        Just EventEndElement{} -> return []
        Nothing -> return []
        _ -> do
            xml <- getSingleXML
            case xml of
                Just xml' -> (xml' :) <$> getXMLList
                Nothing -> return []

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
