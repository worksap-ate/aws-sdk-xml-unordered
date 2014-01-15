{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>), (<*>), Applicative)
import qualified Data.ByteString.Lazy as L
import Data.Conduit (($$), ($=), MonadThrow (..), runResourceT)
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import Test.Hspec
import Text.XML.Stream.Parse (parseLBS, def)

import Cloud.AWS.Lib.Parser.Unordered

import qualified Cloud.AWS.Lib.Parser.Unordered.Types as N
import qualified Cloud.AWS.Lib.Parser.Unordered.Conduit as N
import qualified Cloud.AWS.Lib.Parser.Unordered.Convert as N

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
        it "parse list of text" parseList
        it "cannot parse list of text" parseListFailure
        it "parse top level elements" parseTopElements
        it "parse escaped content" parseEscaped
    describe "xml parser of maybe version" $
        it "parse empty xml" parseEmpty
    describe "xml parser of conduit version" $ do
        it "parse normal xml" parseTopDataSetConduit
        it "parse empty itemSet" parseEmptyItemSetConduit
    describe "new version" $ do
        it "can parse normal xml" parseNewVersion
        it "can parse ec2response-like xml" parseEC2Response

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

parseEmptyItemSetConduit :: Expectation
parseEmptyItemSetConduit = do
    d <- runResourceT $ parseLBS def input $=
        xmlParserConduit "dataSet" (\xml -> getElement xml "data" parseTestData) $$
        CL.consume
    d `shouldBe` input'
  where
    input = L.concat
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        , "<dataSet>"
        , "</dataSet>"
        ]
    input' = []

parseList :: Expectation
parseList = do
    d <- runResourceT $ parseLBS def input $$
        xmlParser (\xml -> getElements xml "dataSet" "data" content)
    d `shouldBe` input'
  where
    input = L.concat
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        , "<dataSet>"
        , "<data>item</data>"
        , "<data>item</data>"
        , "<data>item</data>"
        , "</dataSet>"
        ]
    input' = ["item", "item", "item"]

parseListFailure :: Expectation
parseListFailure = do
    runResourceT $ parseLBS def input $$
        xmlParser (\xml -> getElements xml "dataSet" "data" content)
    `shouldThrow` errorCall "This is not a content."
  where
    input = L.concat
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        , "<dataSet>"
        , "<data><dummy/></data>"
        , "</dataSet>"
        ]

parseTopElements :: Expectation
parseTopElements = do
    d <- runResourceT $ parseLBS def input $$
        xmlParser (\xml -> (,,)
            <$> xml .< "itemA"
            <*> xml .< "itemB"
            <*> getElements xml "itemSet" "item" (\xml' ->
                getElements xml' "itemSet" "item" content
                )
        )
    d `shouldBe` input'
  where
    input = L.concat
        [ "<itemA>item-a</itemA>"
        , "<itemB>item-b</itemB>"
        , "<itemSet>"
        , "  <item>"
        , "    <itemSet>"
        , "      <item>a</item>"
        , "    </itemSet>"
        , "  </item>"
        , "</itemSet>"
        ]
    input' = ("item-a", "item-b", [["a"]]) :: (Text, Text, [[Text]])

parseEscaped :: Expectation
parseEscaped = do
    d <- runResourceT $ parseLBS def input $$ xmlParser (.< "escaped")
    d `shouldBe` input'
  where
    input = "<escaped>{&quot;version&quot;:&quot;1.0&quot;,&quot;queryDate&quot;:&quot;2013-05-08T21:09:40.443+0000&quot;,&quot;startDate&quot;:&quot;2013-05-08T20:09:00.000+0000&quot;,&quot;statistic&quot;:&quot;Maximum&quot;,&quot;period&quot;:3600,&quot;recentDatapoints&quot;:[6.89],&quot;threshold&quot;:90.5}</escaped>"
    input' = "{\"version\":\"1.0\",\"queryDate\":\"2013-05-08T21:09:40.443+0000\",\"startDate\":\"2013-05-08T20:09:00.000+0000\",\"statistic\":\"Maximum\",\"period\":3600,\"recentDatapoints\":[6.89],\"threshold\":90.5}" :: Text

dataConv :: (MonadThrow m, Applicative m) => N.XmlElement -> m TestData
dataConv el = TestData
    <$> el N..< "id"
    <*> el N..< "name"
    <*> el N..< "description"
    <*> N.elements el "itemSet" "item" itemConv

itemConv :: (MonadThrow m, Applicative m) => N.XmlElement -> m TestItem
itemConv el = TestItem
    <$> el N..< "id"
    <*> el N..< "name"
    <*> el N..< "description"
    <*> N.elementM el "subItem" itemConv

parseNewVersion :: Expectation
parseNewVersion = do
    d <- runResourceT $ parseLBS def input $= mapElem $$ sink
    d `shouldBe` input'
  where
    mapElem = N.elementConduit ["data"]
    sink = N.convert (\el -> N.element el "data" dataConv)
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

parseEC2Response :: Expectation
parseEC2Response = do
    (rid, d, nt) <- runResourceT $ parseLBS def (input True False) $= mapElem $$ sink
    rid `shouldBe` Just ("req-id" :: Text)
    d `shouldBe` input'
    nt `shouldBe` (Nothing :: Maybe Text)
    (rid', d', nt') <- runResourceT $ parseLBS def (input False True) $= mapElem $$ sink
    rid' `shouldBe` Nothing
    d' `shouldBe` input'
    nt' `shouldBe` Just "next-token"
  where
    mapElem = N.elementConduit ["requestId", "data", "nextToken"]
    sink = do
        rid <- N.tryConvert (N..< "requestId")
        d <- N.consumeElements $ \el -> N.element el "data" dataConv
        nt <- N.tryConvert (N..< "nextToken")
        return (rid, d, nt)
    input hasReqId hasNextToken = L.concat
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        , "<response>"
        , if hasReqId then "  <requestId>req-id</requestId>" else ""
        , "  <dataSet>"
        , "    <data>"
        , "      <id>1</id>"
        , "      <name>test1</name>"
        , "      <itemSet>"
        , "      </itemSet>"
        , "      <description>this is test</description>"
        , "    </data>"
        , "    <data>"
        , "      <id>2</id>"
        , "      <name>test2</name>"
        , "    </data>"
        , "  </dataSet>"
        , if hasNextToken then "  <nextToken>next-token</nextToken>" else ""
        , "</response>"
        ]
    input' =
        [ TestData
            { testDataId = 1
            , testDataName = "test1"
            , testDataDescription = Just "this is test"
            , testDataItemsSet = []
            }
        , TestData
            { testDataId = 2
            , testDataName = "test2"
            , testDataDescription = Nothing
            , testDataItemsSet = []
            }
        ]
