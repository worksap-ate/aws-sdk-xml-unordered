module Cloud.AWS.Lib.Parser.Unordered.Path
    ( ElementName (..)
    , ElementPath
    , (.=)
    , (.-)
    , tag
    , end
    , anytag
    -- , mergePath
    ) where

import Data.String (IsString (..))
import Data.Text (Text)
import Data.Tree (Tree (..))

import Cloud.AWS.Lib.ToText (ToText (..))

data ElementName = TagName Text | AnyTag deriving (Eq)

instance Show ElementName where
    show (TagName t) = show t
    show AnyTag = "<any>"

instance IsString ElementName where
    fromString = TagName . fromString

type ElementPath = Tree ElementName

infixr 1 .=
-- | infix version of 'Node'
(.=) :: ElementName -> [ElementPath] -> ElementPath
name .= paths = Node name paths
{-# INLINE (.=) #-}

infixr 1 .-
(.-) :: ElementName -> ElementPath -> ElementPath
name .- path = Node name [path]
{-# INLINE (.-) #-}

tag :: ToText a => a -> ElementName
tag a = TagName $ toText a
{-# INLINE tag #-}

end :: ElementName -> ElementPath
end name = Node name []
{-# INLINE end #-}

anytag :: ElementName
anytag = AnyTag
{-# INLINE anytag #-}

-- normalize :: ElementPath -> ElementPath
-- normalize (Node root forest) = Node root $ normalize' forest
--   where
--     normalize' [] = []
--     normalize' fs = if elem AnyTag fs
--         then [AnyTag]
--         else normalize'' fs
--     normalize'' [] = []
--     normalize'' (p:ps) = if elem p ps
--         then normalize'' ps
--         else p : normalize'' ps

-- mergePath :: ElementPath -> ElementPath -> ElementPath
-- mergePath (Node AnyTag f1) (Node AnyTag f2) = AnyTag .- mergePaths f1 f2

-- mergePaths :: [ElementPath] -> [ElementPath] -> [ElementPath]
-- margePaths [] ps = ps
-- mergePaths ps [] = ps
-- mergePaths (AnyTag:p1s) p2s = [AnyTag]
