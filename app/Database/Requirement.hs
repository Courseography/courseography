{-|
Description: Requirement

Module containing data type that represents a "Requirement".

We will use parsed data to create instances of this type.
-}
module Requirement
( ProgramReq(..)
, CourseReq(..)
, Req(..)
, showProgramReq
, showCourseRequisite
, showCourseReq
, showReq
) where

import Data.String


data ProgramReq = ProgramReq String [Req]

-- | Returns a well formatted String representing a Program Requirement for specified Program.
getProgramReq :: ProgramReq -> String
getProgramReq (ProgramReq program reqs) = "The program requirements for " + show program
										+ show $ map (showReq) reqs


-- IMPLEMENT THIS.. MAKE SOME SORT OF INFIX FUNCTION TO GET STRING REPR OF COURSE REQUIREMENTS
-- when initializing this type, we need to check that all the courses correspond correctly.
type CourseRequisite = String :~ (CourseReq, CourseReq, CourseReq)

showCourseRequisite :: CourseRequisite -> String
-- get a list of CourseReqs in order (corequisite, exclusion, prerequisite) and map over to display them.


data CourseReq = CREQ String Req | EXCL String Req | PREQ String Req

-- | Returns a well formatted String representing a String Requirement for specified String.
showCourseReq :: CourseReq -> String
showCourseReq (CREQ course Req) = "Corequisites for" + course + ":/n"
							    + showReq Req + "/n"
showCourseReq (EXCL course Req) = "Exclusions for" + course + ":/n"
							    + showReq Req + "/n"
showCourseReq (PREQ course Req) = "Prerequisites for" + course + ":/n"
							    + showReq Req + "/n"


-- for now J seems to be most readable and convenient value constructor for satisfying rec structure.
data Req = J String | AND [Req]| OR [Req] | FROM (Integer) [Req]

-- | Returns a well formatted String representing the String(s) relationships.
showReqs :: Req -> String
showReqs (J course) = course
showReqs (AND x:xs) = showReqs x + ", " + showReqs (AND xs)
showReqs (OR x:xs) = showReqs x + "/ " + showReqs (OR xs)
showReqs (FROM (a) x:xs) =  show a + "FCE(s) from: (" + showReqs x + ", "
					     +  showReqs (xs) + ")"