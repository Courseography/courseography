module WebParsing.PostParser
    (addPostToDatabase) where

import qualified Data.Text as T
import Control.Monad.Trans (liftIO)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Data.List.Split(splitWhen)
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
        nonEmptyPartitions = if null liPartitions then numberedPartitions else liPartitions
        programPrereqs = map getCourseFromTag $ map (fromAttrib "href") $ filter isCourseTag programElements
        firstCourse = if null programPrereqs then Nothing else (Just (head programPrereqs))
    categoryParser requirements fullPostName firstCourse nonEmptyPartitions
    liftIO (print numberedPartitions)
    where
        isRequirementSection element = tagOpenAttrLit "div" ("class", "field-content") element
        isCourseTag tag = tagOpenAttrNameLit "a" "href" (\hrefValue -> T.isInfixOf "/course" hrefValue) tag
        isLiTag tag = isTagOpenName "li" tag

addPostCategoriesToDatabase :: PostId -> [T.Text] -> SqlPersistM ()
addPostCategoriesToDatabase key categories = do
    mapM_ (addCategoryToDatabase key) (filter isCategory categories)
    where
        isCategory text =
            let infixes = map (containsText text)
                         ["First", "Second", "Third", "suitable", "Core", "Electives"]
            in
                ((T.length text) >= 7) && ((length $ filter (\bool -> bool) infixes) <= 0)
        containsText text subtext = T.isInfixOf subtext text

addCategoryToDatabase :: PostId -> T.Text -> SqlPersistM ()
addCategoryToDatabase key category =
    insert_ $ PostCategory key category

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
            return ()
    where
        parsed = case listPartitions of
            [] -> P.parse (generalCategoryParser fullPostName firstCourse) failedString (innerText tags)
            partitionResults -> do
                post <- P.parse (postInfoParser fullPostName firstCourse) failedString (innerText tags)
                return (post, partitionResults)

parseLi :: [Tag T.Text] -> T.Text
parseLi liPartition = do
    let parsed = P.parse parseCategory failedString (innerText liPartition)
    case parsed of
        Right category -> category
        Left _ -> ""

getNumberedPartitions :: [Tag T.Text] -> [[Tag T.Text]]
getNumberedPartitions tags = 
    let pTags = partitions (isTagOpenName "p") tags
    in concatMap (splitWhen (isTagOpenName "br")) pTags
       

parseNumberedPartition :: [Tag T.Text] -> T.Text
parseNumberedPartition pPartition = do
    let parsed = P.parse parseNumberedLine failedString (innerText pPartition)
    case parsed of
        Right category ->  T.replace "\n" " " $ T.replace "\8203" " " $ T.replace "\160" " " $ T.strip category
        Left _ -> ""
