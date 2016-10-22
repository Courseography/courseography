{-|
Description: Course

Module containing data type that represents a Course.

Use parsed data to create instances of this type.
-}
module Course
( Course(..)
, getCreq
, getExcl
, getPreq
, getCourseReq
) where

import Data.String
import qualified Database.CourseReq as CR

data Course = Course { name :: String
                     , breadth :: String
                     , description :: String
                     , title  :: String
                     , prereqString :: String
                     , fallSession :: String
                     , springSession :: String
                     , yearSession :: String
                     , name :: String
                     , exclusions :: CourseReq
                     , manualTutorialEnrolment :: String
                     , manualPracticalEnrolment :: String
                     , distribution :: String
                     , prereqs :: CourseReq
                     , coreqs :: CourseReq
                     , videoUrls :: []
                     }

-- | Returns a well formatted String representing the Corequisites for specified Course.
getCreq :: Course -> String
getCreq course =  CR.showCourseReq $ coreqs course

-- | Returns a well formatted String representing the Exclusions specified Course.
getExcl :: Course -> String
getExcl course =  CR.showCourseReq $ exclusions course

-- | Returns a well formatted String representing the Prerequisites specified Course.
getPreq :: Course -> String
getPreq course =  CR.showCourseReq $ prereqs course

-- | Returns a well formatted String representing the Course Requirements for specified Course.
getCourseReq :: Course -> String
-- get a list of CourseReqs in order (corequisite, exclusion, prerequisite) and map over to display them.
getCourseReq course = show $ map (CR.showCourseReq) [ x course |  x <- [coreqs, exclusions, prereqs] ] 