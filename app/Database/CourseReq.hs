{-|
Description: Course Requirement

Module containing data type that represents a Course Requirement.

We will use parsed data to create instances of this type.
-}
module CourseReq
( CourseReq(..)
, showCourseReq
) where

import Data.String
import Database.Course
import Database.ReqBuild

data CourseReq = CREQ Course Req | EXCL Course Req
               | PREQ Course Req

showCourseReq :: CourseReq -> String
showCourseReq (CREQ course Req) = "Corequisites for" + name course + ":/n"
							    + showReq Req + "/n"
showCourseReq (EXCL course Req) = "Exclusions for" + name course + ":/n"
							    + showReq Req + "/n"
showCourseReq (PREQ course Req) = "Prerequisites for" + name course + ":/n"
							    + showReq Req + "/n"