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
import WebParsing.ParsecCombinators (getCourseFromTag, generalCategoryParser, parseCategory,
    postInfoParser, parseNumberedLine)

failedString :: String
failedString = "Failed."

addPostToDatabase :: [Tag T.Text] -> SqlPersistM ()
addPostToDatabase programElements = do
    -- TODO: Remove Focuses from programElements
    let fullPostName = innerText $ take 1 $ filter isTagText programElements
        requirements = last $ sections isRequirementSection programElements
        liPartitions = map parseLi $ partitions isLiTag requirements
        numberedPartitions = filter (not . T.null) $ map parseNumberedPartition $ getNumberedPartitions requirements
        nonEmptyPartitions = if null numberedPartitions then liPartitions else numberedPartitions
        programPrereqs = map getCourseFromTag $ map (fromAttrib "href") $ filter isCourseTag programElements
        firstCourse = if null programPrereqs then Nothing else (Just (head programPrereqs))
    categoryParser requirements fullPostName firstCourse nonEmptyPartitions
    liftIO $ print numberedPartitions
    liftIO $ print $ reqHtmlToLines requirements  -- TODO: This is just for debugging purposes, and should be removed.
    where
        isRequirementSection element = tagOpenAttrLit "div" ("class", "field-content") element
        isCourseTag tag = tagOpenAttrNameLit "a" "href" (T.isInfixOf "/course") tag
        isLiTag tag = isTagOpenName "li" tag


-- | Split requirements HTML into individual lines.
reqHtmlToLines :: [Tag T.Text] -> [[T.Text]]
reqHtmlToLines tags =
    let paragraphs = splitWhen (isTagOpenName "p") tags
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

-- Helpers
categoryParser :: [Tag T.Text] -> T.Text -> Maybe T.Text -> [T.Text] -> SqlPersistM ()
categoryParser tags fullPostName firstCourse listPartitions = do
    case parsed of
        Right (post, categories) -> do
            postExists <- insertUnique post
            case postExists of
                Just key -> do
                    addPostCategoriesToDatabase key categories
                Nothing -> return ()
        Left _ -> do
            liftIO $ print failedString
    where
        parsed = case listPartitions of
            [] -> do
                P.parse (generalCategoryParser fullPostName firstCourse) failedString (innerText tags)
            partitionResults -> do
                post <- P.parse (postInfoParser fullPostName firstCourse) failedString (innerText tags)
                return (post, partitionResults)

addPostCategoriesToDatabase :: PostId -> [T.Text] -> SqlPersistM ()
addPostCategoriesToDatabase key categories = do
    mapM_ (insert_ . PostCategory key) (filter isCategory categories)
    where
        isCategory text = T.length text >= 7 && any (flip T.isInfixOf $ text) ["First", "Second", "Third"] --, "suitable", "Core", "Electives"]

parseLi :: [Tag T.Text] -> T.Text
parseLi liPartition =
    case P.parse parseCategory failedString (innerText liPartition) of
        Right c -> c
        Left _ -> ""

getNumberedPartitions :: [Tag T.Text] -> [[Tag T.Text]]
getNumberedPartitions tags = 
    let pTags = partitions (isTagOpenName "p") tags
    in concatMap (splitWhen (isTagOpenName "br")) pTags

parseNumberedPartition :: [Tag T.Text] -> T.Text
parseNumberedPartition pPartition =
    case P.parse parseNumberedLine failedString (innerText pPartition) of
        Right c -> c
        Left _ -> ""
