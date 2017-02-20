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

data ProgramReq = PRGREQ String [Req] deriving (Eq, Show)

-- Returns a well formatted String representing a program requirement for specified program.
-- instance Show ProgramReq where
--     show (PRGREQ program reqs) = "Program Requirements for " ++ program
--                                ++ ":\n" ++ L.intercalate "\n" (map show reqs)

data CourseReq = CRSREQ String Req Req Req deriving (Eq, Show)

-- instance Show CourseReq where
--     show (CRSREQ course creq excl preq) = "Corequisites for " ++ course ++ ":\n"
--                                         ++ show creq ++ "\n" ++ "Exclusions for "
--                                         ++ course ++ ":\n" ++ show excl ++ "\n"
--                                         ++ "Prerequisites for " ++ course ++ ":\n"
--                                         ++ show preq ++ "\n"

-- for now J seems to be most readable and convenient value constructor for satisfying rec structure.
data Req = J String | AND [Req] | OR [Req] | FROM String Req | GRADE String Req | RAW String deriving (Eq, Show)

-- instance Show Req where
--     show (J course) = course
--     show (AND reqs) =
--         case reqs of
--         [x] -> show x
--         otherwise -> "(" ++ (L.intercalate "," $ map show reqs) ++ ")"
--     show (OR reqs) =
--         case reqs of
--         [x] -> show x
--         otherwise -> "(" ++ (L.intercalate "/" $ map show reqs) ++ ")"
--     show (FROM fces reqs) =  fces ++ " FCE(s) from:\n" ++ show reqs
--     show (GRADE grade reqs) =  "(" ++ show reqs ++ " with a minimum grade of " ++ grade ++ "%)"
--     show (RAW text) = "(" ++ text ++ ")"
