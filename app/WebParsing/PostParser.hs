module WebParsing.PostParser
    (addPostToDatabase) where

import qualified Data.Text as T
import Control.Monad.Trans (liftIO)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Data.List.Split (split, splitWhen, whenElt, keepDelimsL)
import Database.Tables
import Database.Persist.Sqlite (insert_, SqlPersistM)
import Database.Persist (insertUnique)
import qualified Text.Parsec as P
import WebParsing.ParsecCombinators (getCourseFromTag, parseCategory,
    postInfoParser)

failedString :: String
failedString = "Failed."

addPostToDatabase :: [Tag T.Text] -> SqlPersistM ()
addPostToDatabase programElements = do
    -- TODO: Remove Focuses from programElements
    let fullPostName = innerText $ take 1 $ filter isTagText programElements
        requirements = last $ sections isRequirementSection programElements
        reqByYear = reqHtmlToLines requirements
        programPrereqs = map getCourseFromTag $ map (fromAttrib "href") $ filter isCourseTag programElements
        firstCourse = if null programPrereqs then Nothing else (Just (head programPrereqs))
    categoryParser requirements reqByYear fullPostName firstCourse
    where
        isRequirementSection element = tagOpenAttrLit "div" ("class", "field-content") element
        isCourseTag tag = tagOpenAttrNameLit "a" "href" (T.isInfixOf "/course") tag

isSectionSplit :: T.Text -> Bool
isSectionSplit txt = any (flip T.isInfixOf $ txt) ["First", "Second", "Third", "Higher", "Notes", "NOTES"]

-- | Split requirements HTML into individual lines.
reqHtmlToLines :: [Tag T.Text] -> [[T.Text]]
reqHtmlToLines tags = 
    let sects = split (keepDelimsL $ whenElt (\t -> (isTagText t) && (isSectionSplit $ fromTagText t))) tags
        sectionsNoNotes = filter (not . isNoteSection) sects
        paragraphs = concatMap (splitWhen (isTagOpenName "p")) sectionsNoNotes
    in 
        map parHtmlToLines paragraphs

-- | Split HTML in a single p tag into lines based on <br> and <li> tags.
parHtmlToLines :: [Tag T.Text] -> [T.Text]
parHtmlToLines tags =
    let lines' = splitWhen (\t -> isTagOpenName "br" t || isTagOpenName "li" t) tags
    in
        map (T.strip . innerText . convertLine) lines'

    where
        convertLine [] = []
        convertLine (t:ts)
            | isTagOpenName "li" t = t : (TagText "0.") : ts
            | otherwise = t:ts

isNoteSection :: [Tag T.Text] -> Bool
isNoteSection section = 
    let sectionTitleTag = head section
    in
        isTagText sectionTitleTag && (any (flip T.isInfixOf $ fromTagText $ sectionTitleTag) ["Notes", "NOTES"])

-- Helpers
categoryParser :: [Tag T.Text] -> [[T.Text]] -> T.Text -> Maybe T.Text -> SqlPersistM ()
categoryParser tags requirements fullPostName firstCourse = do
    let categories = map parseRequirement requirements
    let parsedPost = P.parse (postInfoParser fullPostName firstCourse) failedString (innerText tags)
    liftIO (print categories)
    case parsedPost of
        Right post -> do
            postExists <- insertUnique post
            case postExists of
                Just key -> do
                    addPostCategoriesToDatabase key categories
                Nothing -> return ()
        Left _ -> do
            liftIO $ print failedString

addPostCategoriesToDatabase :: PostId -> [T.Text] -> SqlPersistM ()
addPostCategoriesToDatabase key categories = do
    mapM_ (insert_ . PostCategory key) (filter isCategory categories)
    where
        isCategory text = T.length text >= 7

parseRequirement :: [T.Text] -> T.Text
parseRequirement requirement = do
    T.intercalate ", " (map parseSingleReq (filter isReq requirement))
    where
        isReq text = T.length text >= 7 && not (any (flip T.isInfixOf $ text) ["First", "Second", "Third", "Higher"])

parseSingleReq :: T.Text -> T.Text
parseSingleReq req = do
    let parsed = P.parse parseCategory failedString req
    case parsed of
        Right category -> category
        Left _ -> ""
