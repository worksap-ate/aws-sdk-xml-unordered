{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import Test.Hspec
import Text.XML.Stream.Parse

import Cloud.AWS.Lib.Parser.Unordered

main :: IO ()
main = hspec $ do
    describe "xml parser" $ do
        it "parse normal xml" parseNormal
        it "parse xml which contains unordered elements" parseUnordered
        it "parse xml which contains empty list" parseEmptyList
        it "parse xml which does not contain itemSet tag" parseNotAppearItemSet
        it "cannot parse unexpected xml structure" notParseUnexpectedDataStructure
        it "ignore unexpected tag" ignoreUnexpectedTag
        it "parse top data set" parseTopDataSet
    describe "xml parser of maybe version" $
        it "parse empty xml" parseEmpty
    describe "xml parser of conduit version" $
        it "parse top data set" parseTopDataSetConduit

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
    <$> xml .< "id"
    <*> xml .< "name"
    <*> xml .< "description"
    <*> getElements xml "itemSet" "item" parseTestItem

parseTestItem :: (MonadThrow m, Applicative m) => SimpleXML -> m TestItem
parseTestItem xml = TestItem
    <$> xml .< "id"
    <*> xml .< "name"
    <*> xml .< "description"
    <*> getElementM xml "subItem" parseTestItem

parseNormal :: Expectation
parseNormal = do
    d <- runResourceT $ parseLBS def input $$
        xmlParser (\xml -> getElement xml "data" parseTestData)
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
    d <- runResourceT $ parseLBS def input $$
        xmlParser (\xml -> getElement xml "data" parseTestData)
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
    d <- runResourceT $ parseLBS def input $$
        xmlParserM (\xml -> getElement xml "data" parseTestData)
    d `shouldBe` input'
  where
    input = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
    input' = Nothing

parseEmptyList :: Expectation
parseEmptyList = do
    d <- runResourceT $ parseLBS def input $$
        xmlParser (\xml -> getElement xml "data" parseTestData)
    d `shouldBe` input'
  where
    input = L.concat
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        , "<data>"
        , "  <id>1</id>"
        , "  <name>test</name>"
        , "  <description>this is test</description>"
        , "  <itemSet>"
        , "  </itemSet>"
        , "</data>"
        ]
    input' = TestData
        { testDataId = 1
        , testDataName = "test"
        , testDataDescription = Just "this is test"
        , testDataItemsSet = []
        }

parseNotAppearItemSet :: Expectation
parseNotAppearItemSet = do
    d <- runResourceT $ parseLBS def input $$
        xmlParser (\xml -> getElement xml "data" parseTestData)
    d `shouldBe` input'
  where
    input = L.concat
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        , "<data>"
        , "  <id>1</id>"
        , "  <name>test</name>"
        , "</data>"
        ]
    input' = TestData
        { testDataId = 1
        , testDataName = "test"
        , testDataDescription = Nothing
        , testDataItemsSet = []
        }

notParseUnexpectedDataStructure :: Expectation
notParseUnexpectedDataStructure =
    runResourceT (parseLBS def input $$
        xmlParser (\xml -> getElement xml "data" parseTestData))
        `shouldThrow` errorCall "FromText error: no text name=name"
  where
    input = L.concat
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        , "<data>"
        , "  <id>1</id>"
        , "  <name>"
        , "    <first>foo</first>"
        , "    <last>bar</last>"
        , "  </name>"
        , "</data>"
        ]

ignoreUnexpectedTag :: Expectation
ignoreUnexpectedTag = do
    d <- runResourceT $ parseLBS def input $$
        xmlParser (\xml -> getElement xml "data" parseTestData)
    d `shouldBe` input'
  where
    input = L.concat
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        , "<data>"
        , "  <id>1</id>"
        , "  <unexpectedTag>tag</unexpectedTag>"
        , "  <name>test</name>"
        , "  <itemSet>"
        , "    <unexpectedTag>tag</unexpectedTag>"
        , "    <unexpectedTag>tag</unexpectedTag>"
        , "  </itemSet>"
        , "  <unexpectedTag>tag</unexpectedTag>"
        , "</data>"
        ]
    input' = TestData
        { testDataId = 1
        , testDataName = "test"
        , testDataDescription = Nothing
        , testDataItemsSet = []
        }

parseTopDataSet :: Expectation
parseTopDataSet = do
    d <- runResourceT $ parseLBS def input $$
        xmlParser (\xml -> getElements xml "dataSet" "data" parseTestData)
    d `shouldBe` input'
  where
    input = L.concat
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        , "<dataSet>"
        , "  <data>"
        , "    <id>1</id>"
        , "    <name>test1</name>"
        , "    <itemSet>"
        , "    </itemSet>"
        , "    <description>this is test 1</description>"
        , "  </data>"
        , "  <data>"
        , "    <id>2</id>"
        , "    <name>test2</name>"
        , "  </data>"
        , "</dataSet>"
        ]
    input' =
        [ TestData
            { testDataId = 1
            , testDataName = "test1"
            , testDataDescription = Just "this is test 1"
            , testDataItemsSet = []
            }
        , TestData
            { testDataId = 2
            , testDataName = "test2"
            , testDataDescription = Nothing
            , testDataItemsSet = []
            }
        ]

parseTopDataSetConduit :: Expectation
parseTopDataSetConduit = do
    d <- runResourceT $ parseLBS def input $=
        xmlParserConduit "dataSet" (\xml -> getElement xml "data" parseTestData) $$
        CL.consume
    d `shouldBe` input'
  where
    input = L.concat
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        , "<dataSet>"
        , "  <data>"
        , "    <id>1</id>"
        , "    <name>test1</name>"
        , "    <itemSet>"
        , "    </itemSet>"
        , "    <description>this is test 1</description>"
        , "  </data>"
        , "  <data>"
        , "    <id>2</id>"
        , "    <name>test2</name>"
        , "  </data>"
        , "</dataSet>"
        ]
    input' =
        [ TestData
            { testDataId = 1
            , testDataName = "test1"
            , testDataDescription = Just "this is test 1"
            , testDataItemsSet = []
            }
        , TestData
            { testDataId = 2
            , testDataName = "test2"
            , testDataDescription = Nothing
            , testDataItemsSet = []
            }
        ]
