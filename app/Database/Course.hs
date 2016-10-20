{-|
Description: Course

Module containing data type that represents a Course.

Use parsed data to create instances of this type.
-}
module Course
( Course(..)
, getCourseReqs
) where

import Data.String
import Database.CourseReq

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
                     , prereqs :: Course
                     , coreqs :: CourseReq
                     , videoUrls = []
                     }

-- | Returns Course Requirements of specified Course in readable String format.
getCourseReqs :: Course -> String