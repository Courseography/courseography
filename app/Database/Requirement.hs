{-|
Description: Requirement

Module containing data type that represents a "Requirement".

We will use parsed data to create instances of this type.
-}
module Requirement
( ProgramReq(..)
, CourseReq(..)
, Req(..)
) where

import qualified Data.String as S


data ProgramReq = PRGREQ String [Req]

-- | Returns a well formatted String representing a program requirement for specified program.
instance Show ProgramReq where
    show (PRGREQ program reqs) = "Program Requirements for " ++ program ++ ":\n" ++ map show reqs


data CourseReq = CRSREQ String Req Req Req

instance Show CourseReq where
    show (CREQ course creq excl preq) = "Corequisites for " ++ course ++ ":\n" ++ show req ++ "\n"
                                      ++ "Exclusions for " ++ course ++ ":\n" ++ show req ++ "\n"
                                      ++ "Prerequisites for " ++ course ++ ":\n" ++ show req ++ "\n"


-- for now J seems to be most readable and convenient value constructor for satisfying rec structure.
data Req = J String | AND [Req] | OR [Req] | FROM Integer [Req]

instance Show Req where
    show (J course) = course
    show (AND reqs) = S.intercalate "," $ map show reqs
    show (OR reqs) = S.intercalate "/" $ map show reqs
    show (FROM a x) =  show a ++ "FCE(s) from: (" ++ show x ++ ")"