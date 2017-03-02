{-|
Description: Requirement

Module containing data type that represents a "Requirement".

We will use parsed data to create instances of this type.
-}
module Database.Requirement
( Req(..)
) where

import qualified Data.String as S
import qualified Data.List as L

-- for now J seems to be most readable and convenient value constructor for satisfying rec structure.
data Req = J String | AND [Req] | OR [Req] | FROM String Req | GRADE String Req | RAW String deriving (Eq, Show)