module Cloud.AWS.Lib.Parser.Unordered.Path
    ( (.=)
    , (.-)
    , end
    ) where

import Data.Tree

import Cloud.AWS.Lib.Parser.Unordered.Types

-- | infix version of 'Node'
(.=) :: ElementName -> [ElementPath] -> ElementPath
name .= paths = Node name paths
{-# INLINE (.=) #-}

infixr 1 .-
(.-) :: ElementName -> ElementPath -> ElementPath
name .- path = Node name [path]
{-# INLINE (.-) #-}

end :: ElementName -> ElementPath
end name = Node name []
{-# INLINE end #-}
