{-|
    Module      : Util.Helpers
    Description : Contains general-use helper functions.
-}
module Util.Helpers
    (safeHead,
    safeHeadApply) where

-- | Given a list and a default value, returns the head of the list, or the default value
-- if the list is empty.
safeHead :: a -> [a] -> a
safeHead x [] = x
safeHead _ (x:_) = x

-- | Extracts the head of a list and applies a given function, or returns a default value
-- if the list is empty.
safeHeadApply :: (a -> b) -> b -> [a] -> b
safeHeadApply _ x [] = x
safeHeadApply f _ (x:_) = f x
