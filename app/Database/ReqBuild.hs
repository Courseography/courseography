{-|
Description: Requirement Builder

Module containing data type that represents a Requirement.

We will use parsed data to create instances of this type.
-}
module ReqBuild
( Req(..)
, addReq
) where

import Data.String
import Database.Course

data Req = J Course | AND [Req]| OR [Req] | FROM (Integer) [Req]