{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Conduit
import Data.String
import Data.XML.Types
import Control.Applicative
import System.Environment
import Text.XML.Stream.Parse

import Cloud.AWS.Lib.FromText
import Cloud.AWS.Lib.Parser.Unordered

data Hoge = Hoge
  { hogeId :: Int
  , hogeFugaSet :: [Fuga]
  } deriving (Show)

data Fuga = Fuga
  { fugaName :: Text
  , fugaDesc :: Text
  , fugaFoo :: Maybe Foo
  } deriving (Show)

data Foo = Foo
  { fooBar :: Text
  } deriving (Show)

hoge :: (MonadThrow m, Applicative m) => SimpleXML -> m Hoge
hoge xml = Hoge
  <$> getT xml "id"
  <*> getElements xml "fugaSet" "fuga" fuga

fuga :: (MonadThrow m, Applicative m) => SimpleXML -> m Fuga
fuga xml = Fuga
  <$> getT xml "name"
  <*> getT xml "desc"
  <*> getElementM xml "foo" foo

foo :: (MonadThrow m, Applicative m) => SimpleXML -> m Foo
foo xml = Foo
  <$> getT xml "bar"

hogeTag :: (MonadThrow m, Applicative m) => ConduitM Event o m Hoge
hogeTag = xmlParser $ \xml -> getElement xml "hoge" hoge

main :: IO ()
main = do
  path : [] <- getArgs
  hoge' <- runResourceT $ parseFile def (fromString path) $$ hogeTag
  print hoge'
