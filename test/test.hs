{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>), (<*>), Applicative)
import qualified Data.ByteString.Lazy as L
import Data.Conduit (($$), ($=), MonadThrow (..), runResourceT)
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import Test.Hspec
import Text.XML.Stream.Parse (parseLBS, def)

import Cloud.AWS.Lib.Parser.Unordered

main :: IO ()
main = hspec $ do
    describe "unordered parser" $ do
        it "can parse normal xml" parseNormal
        it "can parse xml which contains unordered elements" parseUnordered
        it "cannot parse empty xml" cannotParseEmpty
        it "can parse xml which contains empty list" parseEmptyList
        it "can parse xml which does not contain itemSet tag" parseNotAppearItemSet
        it "cannot parse unexpected xml structure" cannotParseUnexpectedDataStructure
        it "ignore unexpected tag" ignoreUnexpectedTag
        it "can parse top data set" parseTopDataSet
        it "can parse list of text" parseList
        it "cannot parse list of text" parseListFailure
        it "can parse escaped content" parseEscaped
        it "can parse ec2response-like xml" parseEC2Response
    describe "unordered parser using conduit" $ do
        it "can parse normal xml" parseTopDataSetConduit
        it "can parse empty itemSet" parseEmptyItemSetConduit
        it "can parse ec2response-like xml" parseEC2ResponseConduit

parseError :: Selector ParseError
parseError = const True

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

dataConv :: (MonadThrow m, Applicative m) => XmlElement -> m TestData
dataConv e = TestData
    <$> e .< "id"
    <*> e .< "name"
    <*> e .< "description"
    <*> elements "itemSet" "item" itemConv e

itemConv :: (MonadThrow m, Applicative m) => XmlElement -> m TestItem
itemConv e = TestItem
    <$> e .< "id"
    <*> e .< "name"
    <*> e .< "description"
    <*> elementM "subItem" itemConv e

getElement :: MonadThrow m => L.ByteString -> m XmlElement
getElement input = parseLBS def input $$ elementConsumer

parseNormal :: Expectation
parseNormal = do
    result <- getElement input >>= element "data" dataConv
    result `shouldBe` expectedData
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
    expectedData = TestData
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
    result <- getElement input >>= element "data" dataConv
    result `shouldBe` expectedData
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
    expectedData = TestData
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

cannotParseEmpty :: Expectation
cannotParseEmpty =
    (getElement input >>= element "data" dataConv)
        `shouldThrow` parseError
  where
    input = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"

parseEmptyList :: Expectation
parseEmptyList = do
    result <- getElement input >>= element "data" dataConv
    result `shouldBe` expectedData
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
    expectedData = TestData
        { testDataId = 1
        , testDataName = "test"
        , testDataDescription = Just "this is test"
        , testDataItemsSet = []
        }

parseNotAppearItemSet :: Expectation
parseNotAppearItemSet = do
    result <- getElement input >>= element "data" dataConv
    result `shouldBe` expectedData
  where
    input = L.concat
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        , "<data>"
        , "  <id>1</id>"
        , "  <name>test</name>"
        , "</data>"
        ]
    expectedData = TestData
        { testDataId = 1
        , testDataName = "test"
        , testDataDescription = Nothing
        , testDataItemsSet = []
        }

cannotParseUnexpectedDataStructure :: Expectation
cannotParseUnexpectedDataStructure =
    (getElement input >>= element "data" dataConv)
        `shouldThrow` anyException -- errorCall "FromText error: no text name=name"
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
    result <- getElement input >>= element "data" dataConv
    result `shouldBe` expectedData
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
    expectedData = TestData
        { testDataId = 1
        , testDataName = "test"
        , testDataDescription = Nothing
        , testDataItemsSet = []
        }

parseTopDataSet :: Expectation
parseTopDataSet = do
    result <- getElement input >>= elements "dataSet" "data" dataConv
    result `shouldBe` expectedData
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
    expectedData =
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
    result <- runResourceT $ parseLBS def input $= mapElem $=
        convertConduit (element "data" dataConv) $$
        CL.consume
    result `shouldBe` expectedData
  where
    mapElem = elementConduit $ "dataSet" .- end "data"
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
    expectedData =
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
    result <- runResourceT $ parseLBS def input $= mapElem $=
        convertConduit (element "data" dataConv) $$
        CL.consume
    result `shouldBe` expectedData
  where
    mapElem = elementConduit $ "dataSet" .- end "data"
    input = L.concat
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        , "<dataSet>"
        , "</dataSet>"
        ]
    expectedData = []

parseList :: Expectation
parseList = do
    result <- getElement input >>= elements "dataSet" "data" content
    result `shouldBe` expectedData
  where
    input = L.concat
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        , "<dataSet>"
        , "<data>item</data>"
        , "<data>item</data>"
        , "<data>item</data>"
        , "</dataSet>"
        ]
    expectedData = ["item", "item", "item"] :: [Text]

parseListFailure :: Expectation
parseListFailure =
    (getElement input >>= elements "dataSet" "data" c)
        `shouldThrow` parseError
  where
    c :: XmlElement -> IO Text
    c = content
    input = L.concat
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        , "<dataSet>"
        , "<data><dummy/></data>"
        , "</dataSet>"
        ]

parseEscaped :: Expectation
parseEscaped = do
    result <- getElement input >>= (.< "escaped")
    result `shouldBe` expectedData
  where
    input = "<escaped>{&quot;version&quot;:&quot;1.0&quot;,&quot;queryDate&quot;:&quot;2013-05-08T21:09:40.443+0000&quot;,&quot;startDate&quot;:&quot;2013-05-08T20:09:00.000+0000&quot;,&quot;statistic&quot;:&quot;Maximum&quot;,&quot;period&quot;:3600,&quot;recentDatapoints&quot;:[6.89],&quot;threshold&quot;:90.5}</escaped>"
    expectedData = "{\"version\":\"1.0\",\"queryDate\":\"2013-05-08T21:09:40.443+0000\",\"startDate\":\"2013-05-08T20:09:00.000+0000\",\"statistic\":\"Maximum\",\"period\":3600,\"recentDatapoints\":[6.89],\"threshold\":90.5}" :: Text

parseEC2Response :: Expectation
parseEC2Response = do
    (rid, d, nt) <- getElement (input True False) >>= element "response" conv
    rid `shouldBe` Just "req-id"
    d `shouldBe` expectedData
    nt `shouldBe` Nothing
    (rid', d', nt') <- getElement (input False True) >>= element "response" conv
    rid' `shouldBe` Nothing
    d' `shouldBe` expectedData
    nt' `shouldBe` Just "next-token"
  where
    conv :: (MonadThrow m, Applicative m) => XmlElement -> m (Maybe Text, TestData, Maybe Text)
    conv e = (,,)
        <$> e .< "requestId"
        <*> element "data" dataConv e
        <*> e .< "nextToken"
    input hasReqId hasNextToken = L.concat
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        , "<response>"
        , if hasReqId then "  <requestId>req-id</requestId>" else ""
        , "  <data>"
        , "    <id>1</id>"
        , "    <name>test1</name>"
        , "    <itemSet>"
        , "    </itemSet>"
        , "    <description>this is test</description>"
        , "  </data>"
        , if hasNextToken then "  <nextToken>next-token</nextToken>" else ""
        , "</response>"
        ]
    expectedData = TestData
        { testDataId = 1
        , testDataName = "test1"
        , testDataDescription = Just "this is test"
        , testDataItemsSet = []
        }

parseEC2ResponseConduit :: Expectation
parseEC2ResponseConduit = do
    (rid, d, nt) <- runResourceT $ parseLBS def (input True False) $= mapElem $$ sink
    rid `shouldBe` Just ("req-id" :: Text)
    d `shouldBe` expectedData
    nt `shouldBe` (Nothing :: Maybe Text)
    (rid', d', nt') <- runResourceT $ parseLBS def (input False True) $= mapElem $$ sink
    rid' `shouldBe` Nothing
    d' `shouldBe` expectedData
    nt' `shouldBe` Just "next-token"
  where
    mapElem = elementConduit $ "response" .=
        [ end "requestId"
        , "dataSet" .- end "data"
        , end "nextToken"
        ]
    sink = do
        rid <- tryConvert (.< "requestId")
        d <- convertMany $ element "data" dataConv
        nt <- tryConvert (.< "nextToken")
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
    expectedData =
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
