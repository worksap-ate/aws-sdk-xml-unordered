{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module Cloud.AWS.Lib.Parser.Unordered.Conduit where

import Control.Applicative
import Control.Exception (SomeException)
import Control.Exception.Lifted (try)
import Control.Monad.Trans (lift)
import Data.Char (isSpace)
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.XML.Types

import Cloud.AWS.Lib.Parser.Unordered.Types

-- | map from 'Event' to 'XmlElement'.
elementConduit :: MonadThrow m
              => [ElementName]
              -> Conduit Event m XmlElement
elementConduit names = do
    drops
    e <- CL.peek
    case e of
        Just e' | isTarget e' -> do
            el <- getElement
            maybe (return ()) yield el
            elementConduit names
        Nothing -> return ()
        _ -> do
            CL.drop 1
            elementConduit names
  where
    isTarget (EventBeginElement name _) = elem (nameLocalName name) names
    isTarget _ = False

-- | Drop unnecessary event.
drops :: Monad m => ConduitM Event o m ()
drops = do
    e <- CL.peek
    case e of
        Just e' | unnecessary e' -> CL.drop 1 >> drops
        _ -> return ()
  where
    unnecessary EventBeginElement{} = False
    unnecessary EventEndElement{} = False
    unnecessary (EventContent (ContentText t))
        | T.all isSpace t = True
        | otherwise = False
    unnecessary (EventContent ContentEntity{}) = False
    unnecessary _ = True

getElement :: MonadThrow m
           => ConduitM Event o m (Maybe XmlElement)
getElement = do
    drops
    e <- CL.peek
    case e of
        Just (EventBeginElement name _) -> do
            CL.drop 1
            els <- getElements
            let el = HM $ HM.singleton (nameLocalName name) $ maybeToList $ foldElements els
            drops
            e' <- CL.peek
            case e' of
                Just (EventEndElement name')
                    | name == name' -> CL.drop 1 >> return (Just el)
                _ -> lift $ monadThrow $ ParseError ("getElement: expected end tag: " <> T.pack (show name)) -- arienai
        Just (EventContent (ContentText t)) -> CL.drop 1 >> return (Just $ T t)
        _ -> return Nothing

getElements :: MonadThrow m
            => ConduitM Event o m [XmlElement]
getElements = do
    drops
    e <- CL.peek
    case e of
        Just EventEndElement{} -> return []
        Nothing -> return []
        _ -> getElement >>= maybe (return []) (\el -> (el :) <$> getElements)

foldElements :: [XmlElement] -> Maybe XmlElement
foldElements [] = Nothing
foldElements els@(T _ : _) = Just . T . T.concat . map extract $ els
  where
    extract (T t) = t
    extract _ = error $ "foldElements: Unexpected structure. Please report. " ++ show els
foldElements els@(HM _ : _)
    | hm == HM.empty = Nothing
    | otherwise = Just $ HM hm
  where
    hm = foldr (HM.unionWith (++) . extract) HM.empty els
    extract (HM hmap) = hmap
    extract _ = error $ "foldelements: Unexpected structure. Please report. " ++ show els

convert :: MonadThrow m => (XmlElement -> m a) -> ConduitM XmlElement o m a
convert conv = await >>= maybe (lift . monadThrow $ ParseError "convert: no element") (lift . conv)

-- | if conversion is success, it consume a element. otherwise, it does not consume any elements.
tryConvert :: MonadBaseControl IO m
           => (XmlElement -> m a)
           -> ConduitM XmlElement o m (Maybe a)
tryConvert conv = await >>= maybe none (\el ->
    lift (tryMaybe $ conv el) >>= maybe (leftover el >> none) (return . Just))
  where
    none = return Nothing
    tryMaybe m = do
        ei <- try m
        case ei of
            Left (_ :: SomeException) -> return Nothing
            Right a -> return $ Just a

consumeElements :: MonadBaseControl IO m
                => (XmlElement -> m a)
                -> ConduitM XmlElement o m [a]
consumeElements conv = tryConvert conv >>= maybe (return []) (\a -> (a :) <$> consumeElements conv)
