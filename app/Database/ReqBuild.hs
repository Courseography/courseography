{-|
Description: Requirement Builder

Module containing data type that represents a Requirement.

We will use parsed data to create instances of this type.
-}
module ReqBuild
( Req(..)
, showReqs
) where

import Data.String
import qualified Database.Course as C

-- for now J seems to be most readable and convenient value constructor for satisfying rec structure.
data Req = J Course | AND [Req]| OR [Req] | FROM (Integer) [Req]

-- | Returns a well formatted String representing the Course(s) relationships.
showReqs :: Req -> String
showReqs (J course) = C.name course
showReqs (AND x:xs) = showReqs x + ", " + showReqs (AND xs)
showReqs (OR x:xs) = showReqs x + "/ " + showReqs (OR xs)
showReqs (FROM (a) x:xs) =  show a + "FCE(s) from: (" + showReqs x + ", "
					     +  showReqs (xs) + ")"