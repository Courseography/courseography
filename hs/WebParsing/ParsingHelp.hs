{-# LANGUAGE OverloadedStrings #-}
module WebParsing.ParsingHelp
   (CoursePart,
    (-:),
    (~:),
    emptyCourse,
    preProcess,
    replaceAll,
    tagContains,
    dropBetween,
    dropBetweenAll,
    dropAround,
    isCourse,
    isCancelled,
    lowerTag,
    notCancelled,
    parseDescription,
    parseCorequisite,
    parsePrerequisite,
    parseExclusion,
    parseRecommendedPrep,
    parseDistAndBreadth) where

import Text.Regex.Posix ((=~))
import Text.HTML.TagSoup
import qualified Data.Text as T
import Database.Tables
import WebParsing.PrerequisiteParsing

type CoursePart = ([Tag T.Text], Course)


(-:) :: a -> (a -> a) -> a
coursepart -: fn = fn coursepart

(~:) :: CoursePart -> ([Tag T.Text] -> [Tag T.Text]) -> CoursePart
(tags, course) ~: fn =  (fn tags, course)

emptyCourse :: Course
emptyCourse = Course {
                    breadth = Nothing,
                    description = Nothing,
                    title  = Nothing,
                    prereqString = Nothing,
                    fallSession = Nothing,
                    springSession = Nothing,
                    yearSession = Nothing,
                    name = "",
                    exclusions = Nothing,
                    manualTutorialEnrolment = Nothing,
                    manualPracticalEnrolment = Nothing,
                    distribution = Nothing,
                    prereqs = Nothing,
                    coreqs = Nothing,
                    videoUrls = []}

replaceAll :: [T.Text] -> T.Text -> T.Text -> T.Text
replaceAll matches replacement str =
    foldl (\foldStr match-> T.replace match replacement foldStr) str matches

{------------------------------------------------------------------------------
INPUT: a tag containing string tagtext, and reg, a regex string
OUTPUT: True if reg can match tagtext
-------------------------------------------------------------------------------}
tagContains :: [T.Text] -> Tag T.Text -> Bool
tagContains matches (TagText tagtext) =
    True == foldl (\bool match -> or [(T.isInfixOf match tagtext), bool]) False matches


--converts all open and closing tags to lowercase.
lowerTag :: Tag T.Text -> Tag T.Text
lowerTag (TagOpen tag attrs) =
    TagOpen (T.toLower tag) (map (\(x, z) -> (T.toLower x, T.toLower z)) attrs)
lowerTag (TagClose tag) = TagClose (T.toLower tag)
lowerTag text = text

--returns true if the the row contains a cancelled lecture or tutorial
isCancelled :: [T.Text] -> Bool
isCancelled row =
    foldl (\bool text -> bool || T.isPrefixOf "Cancel" text) False row

notCancelled :: [T.Text] -> Bool
notCancelled row = not (isCancelled row)

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
    Just $ T.strip $ (replaceAll str "" (T.concat (map fromTagText tags)))

{------------------------------------------------------------------------------
------------------------------------------------------------------------------}
replaceBetween :: Eq a =>  (a -> Bool) -> (a -> Bool) -> [a] -> [a]-> [a]
replaceBetween start end rep lst =
    let (before, rest) = break start lst
        (_, after) = break end rest
    in if (after == [])
       then before
       else (concat [before, rep, (drop 1 after)])

replaceBetweenAll :: Eq a =>  (a -> Bool) -> (a -> Bool) -> [a] -> [a]-> [a]
replaceBetweenAll _ _ _ [] = []
replaceBetweenAll start end rep lst =
   let (before, rest) = break start lst
       (_, after) = break end rest
   in if (or [(rest == []), (after == [])])
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

{------------------------------------------------------------------------------
DROPAROUND
takes a list, returns all elements between the first element satisfies start,
and first element after that satisfies end.
>>dropAround (== 'b') (=='d') "abcde"
>>"c"
------------------------------------------------------------------------------}
dropAround :: Eq a => (a->Bool) -> (a-> Bool) -> [a] -> [a]
dropAround start end lst =
    let dropBefore = tail $ dropWhile  (\x -> (not $ start x)) lst
        takeBetween = takeWhile (\x -> (not $ end x)) dropBefore
    in takeBetween

-------------COURSE PARSING FUNCTIONS------------------------------------------
-------------------------------------------------------------------------------
preProcess :: [Tag T.Text] -> [Tag T.Text]
preProcess tags =
    let nobreaks = filter (/= TagText "\r\n") tags
        removeUseless = filter isntUseless nobreaks
        removeEnrol = filter (\t -> not $ tagContains ["Enrolment Limits:"] t) removeUseless
    in map cleanText removeEnrol
    where
        cleanText (TagText str) = TagText (replaceAll ["\r\n                    ", "\n                    ","\160","\194","\r\n"] "" str)
        isntUseless (TagText str) = not $ T.all (\c -> or [(c == ' '), (c =='\n')]) str

parseDescription :: CoursePart -> CoursePart
parseDescription (tags, course) =
    let (parsed, rest) = tagBreak ["Prerequisite","Corequisite","Exclusion","Recommended","Distribution","Breadth"] tags
        descriptn = makeEntry parsed Nothing
    in (rest, course {description = descriptn})

parsePrerequisite :: CoursePart -> CoursePart
parsePrerequisite (tags, course) =
    let (parsed, rest) = tagBreak ["Corequisite","Exclusion","Recommended","Distribution","Breadth"] tags
        prereqstr = makeEntry parsed (Just ["Prerequisite:"])
        prereq = parsePrerequisites prereqstr
    in (rest, course {prereqString = prereqstr, prereqs = prereq})

parseCorequisite :: CoursePart -> CoursePart
parseCorequisite (tags, course)  =
    let (parsed, rest) = tagBreak ["Exclusion","Recommended","Distribution","Breadth"] tags
        coreq = makeEntry parsed (Just ["Corequisite:"])
    in (rest, course {coreqs = coreq})

parseExclusion :: CoursePart -> CoursePart
parseExclusion (tags, course) =
    let (parsed, rest) = tagBreak ["Recommended","Distribution","Breadth"] tags
        ex = makeEntry parsed (Just ["Exclusion:"])
    in (rest, course {exclusions = ex})

parseRecommendedPrep :: CoursePart -> CoursePart
parseRecommendedPrep (tags, course) =
    let (_, rest) = tagBreak ["Distribution","Breadth"] tags
    in (rest, course)

parseDistAndBreadth :: CoursePart -> CoursePart
parseDistAndBreadth (tags, course) =
    let dist = makeEntry (Just (filter (tagContains ["Distribution"]) tags)) (Just ["Distribution Requirement Status: "])
        brdth = makeEntry (Just (filter (tagContains ["Breadth"]) tags)) (Just ["Breadth Requirement: "])
    in (tail $ tail tags, course {distribution = dist, breadth = brdth})


-- | returns true if text is a valid course code.
isCourse :: T.Text -> Bool
isCourse text =
    let pat = "[A-Z]{3}[0-9]{3}[HY][0-9]" :: String
    in (T.unpack text) =~ pat
