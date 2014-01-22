{-# LANGUAGE DeriveDataTypeable #-}

module Cloud.AWS.Lib.Parser.Unordered.Types where

import Control.Exception (Exception)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Typeable (Typeable)

data XmlElement = HM (HashMap Text [XmlElement])
                | T Text
                deriving (Show, Eq)

data ParseError = ParseError
    { parseErrorMessage :: Text
    } deriving (Show, Typeable)

instance Exception ParseError

type ElementName = Text

-- data ElementPath =
