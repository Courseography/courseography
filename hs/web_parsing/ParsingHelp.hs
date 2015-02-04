module ParsingHelp 
  (regReplace,
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
tagContains reg (TagText tagtext) = tagtext =~ reg

{------------------------------------------------------------------------------
INPUT a coursePart with empty course record
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
INPUT: a CoursePart with title fields filled
OUTPUT: a CoursePart, where the course record now contains description info
------------------------------------------------------------------------------}
parseDescription :: CoursePart -> CoursePart
parseDescription (tags, course) = 
  --want to take everything before the next field, and since Prerequisites and Exlusion are optional,
  --we have to make sure 
  let descriptags = takeWhile notNextTags tags
      descriptn = (Just (T.pack (concat (map fromTagText descriptags))))
      restofTags = dropWhile notNextTags tags
  in (restofTags, course {description = descriptn})
  where notNextTags x = not $ (tagContains "Prerequisite|Exclusion|Distribution" x)

{-----------------------------------------------------------------------------
INPUT: a CoursePart with title and description fields filled
OUTPUT: A CoursePart, where the course record now contains prerequisite info
-----------------------------------------------------------------------------}
parsePrerequisite :: CoursePart -> CoursePart
parsePrerequisite (tags, course) = 
  let prereqTags = takeWhile notNextTags tags
      cleanedTags = map cleanTag prereqTags
      restOfTags = dropWhile notNextTags tags
      prereqstr = (Just (T.pack (concat (map fromTagText cleanedTags))))
  in  if prereqTags == []
      then (tags, course)
      else (restOfTags, course {prereqString = prereqstr})  
  where cleanTag (TagText s) = TagText (replace "Prerequisite:\r\n                   " " " s)
        notNextTags x = not $ (tagContains "Exclusion|Distribution" x)

{-----------------------------------------------------------------------------
-----------------------------------------------------------------------------}
parseExclusion :: CoursePart -> CoursePart
parseExclusion coursepart = coursepart

{-----------------------------------------------------------------------------
-----------------------------------------------------------------------------}
parseDistribution :: CoursePart -> CoursePart
parseDistribution coursepart = coursepart

{-----------------------------------------------------------------------------
-----------------------------------------------------------------------------}
parseBreadth :: CoursePart -> CoursePart
parseBreadth coursepart = coursepart 