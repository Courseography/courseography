{-|
Description: Requirement

Module containing data type that represents a "Requirement".

We will use parsed data to create instances of this type.
-}
module Database.Requirement
( ProgramReq(..)
, CourseReq(..)
, Req(..)
) where

import qualified Data.String as S
import qualified Data.List as L

data ProgramReq = PRGREQ String [Req]

-- | Returns a well formatted String representing a program requirement for specified program.
instance Show ProgramReq where
    show (PRGREQ program reqs) = "Program Requirements for " ++ program ++ ":\n" ++ L.intercalate "\n" (map show reqs)

data CourseReq = CRSREQ String Req Req Req

instance Show CourseReq where
    show (CRSREQ course creq excl preq) = "Corequisites for " ++ course ++ ":\n" ++ show creq ++ "\n"
                                      ++ "Exclusions for " ++ course ++ ":\n" ++ show excl ++ "\n"
                                      ++ "Prerequisites for " ++ course ++ ":\n" ++ show preq ++ "\n"

-- for now J seems to be most readable and convenient value constructor for satisfying rec structure.
data Req = J String | AND [Req] | OR [Req] | FROM Integer [Req]

instance Show Req where
    show (J course) = course
    show (AND reqs) = L.intercalate "," $ map show reqs
    show (OR reqs) = L.intercalate "/" $ map show reqs
    show (FROM a reqs) =  show a ++ "FCE(s) from: (" ++ show reqs ++ ")"
