{-|
    Module      : Util.Helpers
    Description : Contains general-use helper functions.
-}
module Util.Helpers
    (safeHead) where

-- | Given a list and a default value, returns the head of the list, or the default value
-- if the list is empty.
safeHead :: a -> [a] -> a
safeHead x [] = x
safeHead _ (x:_) = x
