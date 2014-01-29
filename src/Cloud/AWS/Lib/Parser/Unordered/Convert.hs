{-# LANGUAGE OverloadedStrings #-}

module Cloud.AWS.Lib.Parser.Unordered.Convert
    ( (.<)
    , content
    , element
    , elementM
    , elements
    , lookupTag
    ) where

import Control.Monad
import Data.Conduit
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Monoid
import Data.Text (Text)

import Cloud.AWS.Lib.FromText (FromText (..))
import Cloud.AWS.Lib.Parser.Unordered.Types

getContentText :: XmlElement -> Maybe Text
getContentText (HM _) = Nothing
getContentText (T c) = return c

getSubElements :: XmlElement -> Text -> [XmlElement]
getSubElements (HM hm) name = cat $ HM.lookup name hm
  where
    cat (Just xs) = xs
    cat Nothing = []
getSubElements (T _) _ = []

getSubElement :: XmlElement -> Text -> Maybe XmlElement
getSubElement el = listToMaybe . getSubElements el

-- | the operator like aeson's (.:).
(.<) :: (MonadThrow m, FromText a) => XmlElement -> Text -> m a
(.<) xml name = fromNamedText name $
    getSubElement xml name >>= getContentText

-- | content "<tag>tag-content</tag>" === "tag-content"
content :: (MonadThrow m, FromText t) => XmlElement -> m t
content (T t) = fromText t
content _ = monadThrow $ ParseError "This is not a content."

-- | 'elementM conv name el' return Nothing if 'el' doesn't have any elements named "name". otherwise, return 'Just a'.
elementM :: MonadThrow m
         => Text -- ^ tag name
         -> (XmlElement -> m a) -- ^ Conversion function
         -> XmlElement -- ^ element
         -> m (Maybe a)
elementM name conv el = maybe
    (return Nothing)
    (liftM Just . conv)
    (getSubElement el name)

-- | This function throws error if the result of 'elementM' is Nothing.
element :: MonadThrow m
        => Text -- ^ tag name
        -> (XmlElement -> m a) -- ^ conversion function
        -> XmlElement -- ^ element
        -> m a
element name conv el = elementM name conv el >>= maybe
    (monadThrow $ ParseError $ "element: element '" <> name <> "' not found")
    return

elements :: MonadThrow m
         => Text -- ^ name of sets
         -> Text -- ^ name of item
         -> (XmlElement -> m a) -- ^ convert function
         -> XmlElement -- ^ element
         -> m [a]
elements setname itemname conv el = maybe
    (return [])
    (mapM conv . flip getSubElements itemname)
    (getSubElement el setname)

lookupTag :: MonadThrow m
          => Text
          -> XmlElement
          -> m XmlElement
lookupTag name el = case getSubElements el name of
    [e] -> return e
    [] -> monadThrow $ ParseError $ "lookupTag: tag '" <> name <> "' not found"
    _ -> monadThrow $ ParseError $ "lookupTag: tag '" <> name <> "' is list. please use lookupTags."
