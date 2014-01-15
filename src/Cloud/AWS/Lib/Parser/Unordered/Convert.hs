{-# LANGUAGE OverloadedStrings #-}

module Cloud.AWS.Lib.Parser.Unordered.Convert
    ( (.<)
    , content
    , element
    , elementM
    , elements
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

-- | a operator like aeson's (.:).
(.<) :: (MonadThrow m, FromText a) => XmlElement -> Text -> m a
(.<) xml name = fromNamedText name $
    getSubElement xml name >>= getContentText

content :: (MonadThrow m, FromText t) => XmlElement -> m t
content (T t) = fromText t
content _ = monadThrow $ ParseError "This is not a content."

elementM :: MonadThrow m => XmlElement -> Text -> (XmlElement -> m a) -> m (Maybe a)
elementM el name conv = maybe
    (return Nothing)
    (liftM Just . conv)
    (getSubElement el name)

element :: MonadThrow m => XmlElement -> Text -> (XmlElement -> m a) -> m a
element el name conv = elementM el name conv >>= maybe
    (monadThrow $ ParseError $ "element: element '" <> name <> "' not found")
    return

elements :: MonadThrow m => XmlElement -> Text -> Text -> (XmlElement -> m a) -> m [a]
elements el setname itemname conv = maybe
    (return [])
    (mapM conv . flip getSubElements itemname)
    (getSubElement el setname)
