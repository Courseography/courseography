module ParsingHelp 
  (getCourse,
    regReplace,
    tagContains,
    tagContainsAny,
    parseTitle,
    parseDescription,
    parsePrerequisite) where
import Text.Regex.Posix
import Text.HTML.TagSoup
import Data.List
import qualified Data.Text as T
import Data.List.Utils
import Data.Maybe
import Tables


type CoursePart = ([Tag String], Course)

{-
preProcess :: [Tag String] -> [Tag String]
preprocess tags =
  filter (~/= TagText "\r\n") tags 
  regReplace "(\160)*" " "
-}


getCourse :: CoursePart -> Course
getCourse (_, course) = course
{------------------------------------------------------------------------------
INPUT: reg is a regex string, str is a string
OUTPUT: string where every instance of reg in str is replaced

WARNING: currently does 
------------------------------------------------------------------------------}
regReplace :: String -> String -> String -> String
regReplace reg replacement str =
    let match = (str =~ reg)
    in
        if match == ""
        then str
        else regReplace reg  replacement (replace match replacement str)

{------------------------------------------------------------------------------
INPUT: a tag containing string tagtext, and reg, a regex string
OUTPUT: True if reg can match tagtext
-------------------------------------------------------------------------------}
tagContains :: String -> Tag String -> Bool
tagContains str (TagText tagtext) = contains str tagtext 

{------------------------------------------------------------------------------
INPUT: a list of strings str, a tag tag
OUTPUT: True if the text contained in tag contains any of the strings in strs
------------------------------------------------------------------------------}
tagContainsAny :: [String] -> Tag String -> Bool
tagContainsAny strs tag = or (map (\str -> tagContains str tag) strs)

{------------------------------------------------------------------------------
INPUT a tag
OUTPUT: A CoursePart with a course record that has title and name filled in

assumes all TagText "\r\n" have been removed
assumes all \160 have been removed
------------------------------------------------------------------------------}
parseTitle :: CoursePart -> CoursePart
parseTitle (title:tags, course) =  
	let courseNames = splitAt 8 $ removeTitleGarbage $ removeLectureSection $ title
  in (tags, course {title  = (Just (T.pack $ drop 1 $ snd courseNames)), 
                    name = T.pack $ fst courseNames})
  where removeLectureSection (TagText s) = takeWhile (/= '[') s
        removeTitleGarbage s = replace "\160\160\160\160" " " s


{------------------------------------------------------------------------------
INPUT: a CourseParser
OUTPUT: a CourseParser, where the course record now contains 
------------------------------------------------------------------------------}
parseDescription :: CoursePart -> CoursePart
parseDescription (tags, course) = 
  --want to take everything before the next field, and since Prerequisites and Exlusion are optional,
  --we have to make sure 
  let descriptags = takeWhile (\tag -> not (tagContainsAny ["Prerequisite", "Exclusion", "Distribution"] tag)) tags
      descriptn = (Just (T.pack (concat (map fromTagText descriptags))))
      restofTags = dropWhile (\tag -> not (tagContainsAny ["Prerequisite", "Exclusion", "Distribution"] tag)) tags
  in (restofTags, course {description = descriptn})

{-----------------------------------------------------------------------------

STILL NEED TO PUT IN PREREQ FIELD
-----------------------------------------------------------------------------}
parsePrerequisite :: CoursePart -> CoursePart
parsePrerequisite (tags, course) = 
  let prereqTags = takeWhile (\x -> not (tagContainsAny ["Exclusion", "Distribution"] x)) tags
      cleanedTags = map cleanTag prereqTags
      restOfTags = dropWhile (\x -> not (tagContainsAny ["Exclusion", "Distribution"] x)) tags
      prereqstr = (Just (T.pack (concat (map fromTagText cleanedTags))))
  in  if prereqTags == []
      then (tags, course)
      else (restOfTags, course {prereqString = prereqstr})  
  where cleanTag (TagText s) = TagText (replace "Prerequisite:\r\n                   " " " s)

{-----------------------------------------------------------------------------
-----------------------------------------------------------------------------}
--parseExclusion :: CoursePart -> CoursePart


{-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
parseDistribution :: CoursePart -> CoursePart
-}