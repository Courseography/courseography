{-# LANGUAGE OverloadedStrings #-}
module WebParsing.ParsingHelp 
  ( CoursePart,
    (-:),
    (~:),
    preProcess, 
    replaceAll, 
    tagContains,
    dropBetween,
    dropBetweenAll,
    parseDescription,
    parseCorequisite,
    parsePrerequisite,
    parseExclusion,
    parseRecommendedPrep,
    parseDistAndBreadth,
    ) where
import Text.Regex.Posix
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Data.List
import qualified Data.Text as T
import Data.List.Utils
import Data.Maybe
import Database.Tables

type CoursePart = ([Tag T.Text], Course)
type TagParser = (Maybe [Tag T.Text], [Tag T.Text])


(-:) :: CoursePart -> (CoursePart -> CoursePart) -> CoursePart
coursepart -: fn = fn coursepart

(~:) :: CoursePart -> ([Tag T.Text] -> [Tag T.Text]) -> CoursePart
(tags, course) ~: fn =  (fn tags, course)


replaceAll :: [T.Text] -> T.Text -> T.Text -> T.Text
replaceAll matches replacement str = 
  foldl (\str match-> T.replace match replacement str) str matches

{------------------------------------------------------------------------------
INPUT: a tag containing string tagtext, and reg, a regex string
OUTPUT: True if reg can match tagtext
-------------------------------------------------------------------------------}
tagContains :: [T.Text] -> Tag T.Text -> Bool
tagContains matches (TagText tagtext) = 
  True == foldl (\bool match -> or [(T.isInfixOf match tagtext), bool]) False matches

{------------------------------------------------------------------------------
splits a list of tags into two pieces at the first matching instance of reg.
e.g >>tagBreak "hehe" [TagtText "lol", TagText "lmao", TagText "hehe]
    >>([TagText "lol", TagText "lmao"], [TagText "hehe"])
if no matches occur
    >>tagBreak "wat" [TagtText "lol", TagText "lmao", TagText "hehe]
    >>([TagtText "lol", TagText "lmao", TagText "hehe], [])
-------------------------------------------------------------------------------}
tagBreak ::  [T.Text] -> [Tag T.Text] -> (Maybe [Tag T.Text], [Tag T.Text])
tagBreak reg tags = 
  let first = takeWhile (\tag -> not $ tagContains reg tag) tags 
      second = dropWhile (\tag -> not $ tagContains reg tag) tags
  in if first == []
     then (Nothing, second)
     else (Just first, second)

{------------------------------------------------------------------------------
takes in a possible list of tags that represent a specific field in a Course
Record. removes instances of str, and concatenates all tagText into a single
Text entity.
If nothing needs to be removed, pass Nothing as the Text
------------------------------------------------------------------------------}
makeEntry :: Maybe [Tag T.Text] -> Maybe [T.Text] -> Maybe T.Text
makeEntry Nothing _ = Nothing
makeEntry (Just []) _ = Nothing
makeEntry (Just tags) Nothing = Just (T.concat (map fromTagText tags)) 
makeEntry (Just tags) (Just str) = 
  Just (replaceAll str "" (T.concat (map fromTagText tags)))

{------------------------------------------------------------------------------
------------------------------------------------------------------------------}
replaceBetween :: Eq a =>  (a -> Bool) -> (a -> Bool) -> [a] -> [a]-> [a]
replaceBetween start end rep lst =
  let (before, rest) = break start lst
      (between, after) = break end rest 
  in if (after == [])
     then before
     else (concat [before, rep, (drop 1 after)])

replaceBetweenAll :: Eq a =>  (a -> Bool) -> (a -> Bool) -> [a] -> [a]-> [a]
replaceBetweenAll _ _ _ [] = []
replaceBetweenAll start end rep lst = 
  let (before, rest) = break start lst
      (between, after) = break end rest 
  in  if (or [(rest == []), (after == [])])
      then lst 
      else (concat [before, rep, (replaceBetweenAll start end rep (drop 1 after))])

{------------------------------------------------------------------------------
DROPBETWEEN removes all elements between the first element satisfying start, 
and the first element satisfying end inclusive. 
>>dropBetween (== 'b') (== 'd') "abcde"
>>"ae"
------------------------------------------------------------------------------}
dropBetween :: Eq a =>  (a -> Bool) -> (a -> Bool) -> [a] -> [a]
dropBetween start end lst = replaceBetween start end [] lst

dropBetweenAll :: Eq a => (a -> Bool) -> (a -> Bool) -> [a] -> [a]
dropBetweenAll start end lst = replaceBetweenAll start end [] lst

-------------COURSE PARSING FUNCTIONS------------------------------------------
-------------------------------------------------------------------------------
preProcess :: [Tag T.Text] -> [Tag T.Text]
preProcess tags = 
  let nobreaks = filter (/= TagText "\r\n") tags
      removeUseless = filter isntUseless nobreaks 
      removeEnrol = filter (\t -> not $ tagContains ["Enrolment Limits:"] t) removeUseless
  in map cleanText removeEnrol
  where
    cleanText (TagText s) = TagText (replaceAll ["\r\n                    ", "\n                    ","\160","\194","\r\n"] "" s)
    isntUseless (TagText s) = not $ T.all (\c -> or [(c == ' '), (c =='\n')]) s

parseDescription :: CoursePart -> CoursePart
parseDescription (tags, course) = 
  let (parsed, rest) = tagBreak ["Corequisite","Prerequisite","Exclusion","Recommended","Distribution","Breadth"] tags
      descriptn = makeEntry parsed Nothing
  in (rest, course {description = descriptn})

parsePrerequisite :: CoursePart -> CoursePart
parsePrerequisite (tags, course) = 
  let (parsed, rest) = tagBreak ["Corequisite","Exclusion","Recommended","Distribution","Breadth"] tags
      prereqstr = makeEntry parsed (Just ["Prerequisite:"])
  in  (rest, course {prereqString = prereqstr})

parseCorequisite :: CoursePart -> CoursePart
parseCorequisite (tags, course)  = 
  let (parsed, rest) = tagBreak ["Exclusion","Recommended","Distribution","Breadth"] tags
  in (rest, course)

parseExclusion :: CoursePart -> CoursePart
parseExclusion (tags, course) =
  let (parsed, rest) = tagBreak ["Recommended","Distribution","Breadth"] tags
      ex = makeEntry parsed (Just ["Exclusion:"])
  in (rest, course {exclusions = ex})

parseRecommendedPrep :: CoursePart -> CoursePart
parseRecommendedPrep (tags, course) = 
  let (parsed, rest) = tagBreak ["Distribution","Breadth"] tags
  in (rest, course)

parseDistAndBreadth :: CoursePart -> CoursePart
parseDistAndBreadth (tags, course) =
  let dist = makeEntry (Just (filter (tagContains ["Distribution"]) tags)) (Just ["Distribution Requirement Status: "]) 
      brdth = makeEntry (Just (filter (tagContains ["Breadth"]) tags)) (Just ["Breadth Requirement: "])
  in (tail $ tail tags, course {distribution = dist, breadth = brdth})   


