{-|
    Module      : Database.Requirement
    Description : Requirement

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

-- | Defines a program requirement where the first String is the program, and
-- the list of Req is a list of program requirements.
data ProgramReq = PRGREQ String [Req]

-- Returns a well formatted String representing a program requirement for specified program.
instance Show ProgramReq where
    show (PRGREQ program reqs) = "Program Requirements for " ++ program
                               ++ ":\n" ++ L.intercalate "\n" (map show reqs)

-- | Defines a course requirement where the first String is the course, the
-- first requirement is the corequisites, the second requirement is the
-- exlusions, and the third is the prerequisites requirement.
data CourseReq = CRSREQ String Req Req Req

instance Show CourseReq where
    show (CRSREQ course creq excl preq) = "Corequisites for " ++ course ++ ":\n"
                                        ++ show creq ++ "\n" ++ "Exclusions for "
                                        ++ course ++ ":\n" ++ show excl ++ "\n"
                                        ++ "Prerequisites for " ++ course ++ ":\n"
                                        ++ show preq ++ "\n"

-- for now J seems to be most readable and convenient value constructor for satisfying rec structure.
-- | Defines a requirement data type, it should be noted that it can be a
-- course requirement or a program requirement.
data Req = J String | AND [Req] | OR [Req] | FROM String Req | GRADE String Req | RAW String

instance Show Req where
    show (J course) = course
    show (AND reqs) =
        case reqs of
        [x] -> show x
        _ -> "(" ++ L.intercalate "," (map show reqs) ++ ")"
    show (OR reqs) =
        case reqs of
        [x] -> show x
        otherwise -> "(" ++ L.intercalate "/" (map show reqs) ++ ")"
    show (FROM fces reqs) =  fces ++ " FCE(s) from:\n" ++ show reqs
    show (GRADE grade reqs) =  "(" ++ show reqs ++ " with a minimum grade of " ++ grade ++ "%)"
    show (RAW text) = "(" ++ text ++ ")"
