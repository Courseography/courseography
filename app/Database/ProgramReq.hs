{-|
Description: Program Requirement

Module containing data type that represents a Program Requirement.

We will use parsed data to create instances of this type.
-}
module ProgramReq
( ProgramReq(..)
, getProgramReq
) where

import Data.String
import qualified Database.ReqBuild as R

data ProgramReq = ProgramReq String [Req]

-- | Returns a well formatted String representing a Program Requirement for specified Program.
getProgramReq :: ProgramReq -> String
getProgramReq (ProgramReq program reqs) = "The program requirements for " + show program
										+ show $ map (R.showReq) reqs