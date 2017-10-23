module WebParsing.PostParser
    (addPostToDatabase) where

import qualified Data.Text as T
import Control.Monad.Trans (liftIO)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Data.List.Split (splitWhen)
import Database.Tables
import Database.Persist.Sqlite (insert_, SqlPersistM)
import Database.Persist (insertUnique)
import qualified Text.Parsec as P
import WebParsing.ParsecCombinators (getCourseFromTag, parseCategory,
    postInfoParser, parseNumberedLine, parseNotes)

failedString :: String
failedString = "Failed."

addPostToDatabase :: [Tag T.Text] -> SqlPersistM ()
addPostToDatabase programElements = do
    -- TODO: Remove Focuses from programElements
    let fullPostName = innerText $ take 1 $ filter isTagText programElements
        requirements = last $ sections isRequirementSection programElements
        partitions = reqHtmlToLines requirements
        programPrereqs = map getCourseFromTag $ map (fromAttrib "href") $ filter isCourseTag programElements
        firstCourse = if null programPrereqs then Nothing else (Just (head programPrereqs))
    categoryParser requirements partitions fullPostName firstCourse
    --liftIO $ print $ reqHtmlToLines requirements  -- TODO: This is just for debugging purposes, and should be removed.
    --liftIO (print partitions)
    where
        isRequirementSection element = tagOpenAttrLit "div" ("class", "field-content") element
        isCourseTag tag = tagOpenAttrNameLit "a" "href" (T.isInfixOf "/course") tag

-- | Split requirements HTML into individual lines.
reqHtmlToLines :: [Tag T.Text] -> [[T.Text]]
reqHtmlToLines tags = 
    let paragraphs = splitWhen (isTagOpenName "p") tags
        paragraphsWithoutNotes = removeNotes paragraphs
    in 
        map parHtmlToLines paragraphsWithoutNotes

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

-- Helpers
categoryParser :: [Tag T.Text] -> [[T.Text]] -> T.Text -> Maybe T.Text -> SqlPersistM ()
categoryParser tags requirements fullPostName firstCourse = do
    --liftIO (print requirements)
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

removeNotes ::[[Tag T.Text]] -> [[Tag T.Text]]
removeNotes requirements = filter (not . isNote) requirements

isNote :: [Tag T.Text] -> Bool
isNote req = do
    let parsed = P.parse parseNotes failedString (innerText req)
    case parsed of
        Right _ -> True
        Left _ -> False

parseRequirement :: [T.Text] -> T.Text
parseRequirement requirement = do
    T.intercalate ", " (map parseSingleReq (filter isReq requirement))
    where
        isReq text = T.length text >= 7 && not (any (flip T.isInfixOf $ text) ["First", "Second", "Third"]) --, "suitable", "Core", "Electives"]

parseSingleReq :: T.Text -> T.Text
parseSingleReq req = do
    let parsed = P.parse parseCategory failedString req
    case parsed of
        Right category -> category
        Left _ -> ""
