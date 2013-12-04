{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import Data.Text (Text)
import Test.Hspec
import Text.XML.Stream.Parse

import Cloud.AWS.Lib.Parser.Unordered

main :: IO ()
main = hspec $ do
    describe "xml parser" $ do
        it "parse normal xml" parseNormal
        it "parse xml which contains unordered elements" parseUnordered
        it "parse empty xml" parseEmpty

data TestData = TestData
    { testDataId :: Int
    , testDataName :: Text
    , testDataDescription :: Maybe Text
    , testDataItemsSet :: [TestItem]
    } deriving (Eq, Show)

data TestItem = TestItem
    { testItemId :: Int
    , testItemName :: Text
    , testItemDescription :: Maybe Text
    , testItemSubItem :: Maybe TestItem
    } deriving (Eq, Show)

parseTestData :: (MonadThrow m, Applicative m) => SimpleXML -> m TestData
parseTestData xml = TestData
    <$> getT xml "id"
    <*> getT xml "name"
    <*> getT xml "description"
    <*> getElements xml "itemSet" "item" parseTestItem

parseTestItem :: (MonadThrow m, Applicative m) => SimpleXML -> m TestItem
parseTestItem xml = TestItem
    <$> getT xml "id"
    <*> getT xml "name"
    <*> getT xml "description"
    <*> getElementM xml "subItem" parseTestItem

parseNormal :: Expectation
parseNormal = do
    d <- runResourceT $ parseLBS def input $$ xmlParser (\xml -> getElement xml "data" parseTestData)
    d `shouldBe` input'
  where
    input = L.concat
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        , "<data>"
        , "  <id>1</id>"
        , "  <name>test</name>"
        , "  <description>this is test</description>"
        , "  <itemSet>"
        , "    <item>"
        , "      <id>1</id>"
        , "      <name>item1</name>"
        , "      <description>this is item1</description>"
        , "      <subItem>"
        , "        <id>11</id>"
        , "        <name>item1sub</name>"
        , "      </subItem>"
        , "    </item>"
        , "    <item>"
        , "      <id>2</id>"
        , "      <name>item2</name>"
        , "    </item>"
        , "  </itemSet>"
        , "</data>"
        ]
    input' = TestData
        { testDataId = 1
        , testDataName = "test"
        , testDataDescription = Just "this is test"
        , testDataItemsSet =
            [ TestItem
                { testItemId = 1
                , testItemName = "item1"
                , testItemDescription = Just "this is item1"
                , testItemSubItem = Just TestItem
                    { testItemId = 11
                    , testItemName = "item1sub"
                    , testItemDescription = Nothing
                    , testItemSubItem = Nothing
                    }
                }
            , TestItem
                { testItemId = 2
                , testItemName = "item2"
                , testItemDescription = Nothing
                , testItemSubItem = Nothing
                }
            ]
        }

parseUnordered :: Expectation
parseUnordered = do
    d <- runResourceT $ parseLBS def input $$ xmlParser (\xml -> getElement xml "data" parseTestData)
    d `shouldBe` input'
  where
    input = L.concat
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        , "<data>"
        , "  <name>test</name>"
        , "  <itemSet>"
        , "    <item>"
        , "      <name>item1</name>"
        , "      <id>1</id>"
        , "      <subItem>"
        , "        <name>item1sub</name>"
        , "        <id>11</id>"
        , "      </subItem>"
        , "      <description>this is item1</description>"
        , "    </item>"
        , "    <item>"
        , "      <name>item2</name>"
        , "      <id>2</id>"
        , "    </item>"
        , "  </itemSet>"
        , "  <description>this is test</description>"
        , "  <id>1</id>"
        , "</data>"
        ]
    input' = TestData
        { testDataId = 1
        , testDataName = "test"
        , testDataDescription = Just "this is test"
        , testDataItemsSet =
            [ TestItem
                { testItemId = 1
                , testItemName = "item1"
                , testItemDescription = Just "this is item1"
                , testItemSubItem = Just TestItem
                    { testItemId = 11
                    , testItemName = "item1sub"
                    , testItemDescription = Nothing
                    , testItemSubItem = Nothing
                    }
                }
            , TestItem
                { testItemId = 2
                , testItemName = "item2"
                , testItemDescription = Nothing
                , testItemSubItem = Nothing
                }
            ]
        }

parseEmpty :: Expectation
parseEmpty = do
    d <- runResourceT $ parseLBS def input $$ xmlParser (\xml -> getElementM xml "data" parseTestData)
    d `shouldBe` input'
  where
    input = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
    input' = Nothing
