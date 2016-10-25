{-|
Description: Requirement

Module containing data type that represents a "Requirement".

We will use parsed data to create instances of this type.
-}
module Requirement
( ProgramReq(..)
, CourseRequirement(..)
, CourseReq(..)
, Req(..)
, showProgramReq
, showCourseRequirement
, showCourseReq
, showReq
) where

import Data.String


data ProgramReq = PRGREQ String [Req]

-- | Returns a well formatted String representing a program requirement for specified program.
showProgramReq :: ProgramReq -> String
showProgramReq (PRGREQ program reqs) = "The program requirements for " + program + ":\n"
										+ map (showReq) reqs


-- Assuming we have correctly parsed inputs, CourseReq is in order [corequisite, exclusion, prerequisite]
data CourseRequirement = CRSREQ String [CourseReq]

-- | Returns a well formatted String representing all course requirements for specified course.
showCourseRequirement :: CourseRequirement -> String
showCourseRequirement (CRSREQ course coursereqs) = course + "\n" + map (showCourseReq) coursereqs 


data CourseReq = CREQ String Req | EXCL String Req | PREQ String Req

-- | Returns a well formatted String representing a  certain course requirement.
showCourseReq :: CourseReq -> String
showCourseReq (CREQ course req) = "Corequisites for" + course + ":/n"
							    + showReq req + "/n"
showCourseReq (EXCL course req) = "Exclusions for" + course + ":/n"
							    + showReq req + "/n"
showCourseReq (PREQ course req) = "Prerequisites for" + course + ":/n"
							    + showReq req + "/n"


-- for now J seems to be most readable and convenient value constructor for satisfying rec structure.
data Req = J String | AND [Req]| OR [Req] | FROM (Integer) [Req]

-- | Returns a well formatted String representing the course(s) relationships.
showReq :: Req -> String
showReq (J course) = course
showReq (AND x:xs) = showReq x + ", " + showReq (AND xs)
showReq (OR x:xs) = showReq x + "/ " + showReq (OR xs)
showReq (FROM (a) x:xs) =  show a + "FCE(s) from: (" + showReq x + ", "
					     +  showReq (xs) + ")"