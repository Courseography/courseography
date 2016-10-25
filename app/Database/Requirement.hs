{-|
Description: Requirement

Module containing data type that represents a "Requirement".

We will use parsed data to create instances of this type.
-}
module Requirement
( ProgramReq(..)
, CourseRequrement
, CourseReq(..)
, Req(..)
, showProgramReq
, showCourseRequirement
, showCourseReq
, showReq
) where

import Data.String


data ProgramReq = PRGREQ String [Req]

-- | Returns a well formatted String representing a Program Requirement for specified Program.
showProgramReq :: ProgramReq -> String
getProgramReq (ProgramReq program reqs) = "The program requirements for " + program
										+ map (showReq) reqs


-- Assuming we have correctly parsed inputs, CourseReqs in order [corequisite, exclusion, prerequisite]
data CourseRequrement = CRSREQ String [CourseReq]

showCourseRequirement :: CourseRequisite -> String
-- get a list of and map over to display them.
showCourseRequisite (CRSERQ course coursereqs) = course + "\n" + map (showCourseReq) coursereqs 


data CourseReq = CREQ String Req | EXCL String Req | PREQ String Req

-- | Returns a well formatted String representing a String Requirement for specified String.
showCourseReq :: CourseReq -> String
showCourseReq (CREQ course req) = "Corequisites for" + course + ":/n"
							    + showReq req + "/n"
showCourseReq (EXCL course req) = "Exclusions for" + course + ":/n"
							    + showReq req + "/n"
showCourseReq (PREQ course req) = "Prerequisites for" + course + ":/n"
							    + showReq req + "/n"


-- for now J seems to be most readable and convenient value constructor for satisfying rec structure.
data Req = J String | AND [Req]| OR [Req] | FROM (Integer) [Req]

-- | Returns a well formatted String representing the String(s) relationships.
showReqs :: Req -> String
showReqs (J course) = course
showReqs (AND x:xs) = showReqs x + ", " + showReqs (AND xs)
showReqs (OR x:xs) = showReqs x + "/ " + showReqs (OR xs)
showReqs (FROM (a) x:xs) =  show a + "FCE(s) from: (" + showReqs x + ", "
					     +  showReqs (xs) + ")"