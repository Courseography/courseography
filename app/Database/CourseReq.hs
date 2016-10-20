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
import qualified Database.Course as C
import qualified Database.ReqBuild as R

data CourseReq = CREQ Course Req | EXCL Course Req | PREQ Course Req

-- | Returns a well formatted String representing a Course Requirement for specified Course.
showCourseReq :: CourseReq -> String
showCourseReq (CREQ course Req) = "Corequisites for" + C.name course + ":/n"
							    + R.showReq Req + "/n"
showCourseReq (EXCL course Req) = "Exclusions for" + C.name course + ":/n"
							    + R.showReq Req + "/n"
showCourseReq (PREQ course Req) = "Prerequisites for" + C.name course + ":/n"
							    + R.showReq Req + "/n"