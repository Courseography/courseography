{-# LANGUAGE OverloadedStrings #-}

{-|
module          : WebParsing.PrerequisiteParsing
Description     : converts a T.Text representation of prerequisites into prereq format
Stability       : experimental  

Currently parses prerequisites for Arts & Science only. Attempts to preserve only the most
basic internal logic of the prerequisite representation found on course calendar: each element
is either:  1) a 1-element list containig a course name 
            2) an n-element list containing a list of course names.
-}
module WebParsing.PrerequisiteParsing (parsePrerequisites) where 

import Text.Regex.Posix
import qualified Data.Text as T
import Data.List
import Data.List.Utils
import Data.Maybe

{- Signatures:

-}

-- | attempts to match characters used to delimit prerequisite expressions
-- returns (before, delim, after). or (input, "","") if no match occurs
matchDelim :: String -> (String, String, String)
matchDelim prereqs = 
  let pat = "[;,]" :: String
  in prereqs =~ pat
      
-- | returns true if the string begins inside a parenthesized expression.
-- e.g  isntDelim "CSC458)" == True
--      isntDelim "(STA247, STA248)" == False
isntDelim :: String -> Bool
isntDelim rest = 
  let pat = "^.*)" :: String
  in rest =~ pat 

-- |Splits a PrereqString by delimeters ';'' ','. 
toPreExprs :: String -> String -> [String]
toPreExprs str expr  =
  let (before, delim, after) = matchDelim str
  in case (before, delim, after) of 
    ("","","") -> [] --if (expr == "") then [] else [expr]
    (before, "", "") -> [before++expr]
    (before, ",", after) -> if (isntDelim after)
                            then toPreExprs after (expr ++ before)
                            else (expr++before):(toPreExprs after "")
    (before, ";", after) -> (expr++before):(toPreExprs after "")

-- | attempts to match a course in given string. returns (before, course, after)
-- if no match occurs (input, "", "")  
matchCourse :: String -> (String, String, String)
matchCourse prereqs = 
  let pat = "[A-Z]{3}[0-9]{3}[HY][0-9]" :: String
  in prereqs =~ pat

-- | converts a string representing a prerequisite expression into a prerequisite
-- expression. Extracts all course names found within the string, and returns them
-- in a string.
toPrereq :: String -> T.Text
toPrereq expr = 
  let (before, course, after) = matchCourse expr 
  in case (before, course, after) of
     (before, "", "") -> ""
     --guaranteed match 
     (before, course, "") -> T.pack course
     (_, course, after) ->  T.concat [(T.pack course), " ", (toPrereq after)]

-- | converts a text representation of Course prerequisites into type of prereqs field
-- in course record.
parsePrerequisites :: Maybe T.Text -> Maybe T.Text
parsePrerequisites Nothing = Nothing
parsePrerequisites (Just prereqStr) =
  Just $ T.strip $ T.intercalate "," (map toPrereq (toPreExprs (T.unpack prereqStr) ""))


