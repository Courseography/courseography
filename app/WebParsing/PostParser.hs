module WebParsing.PostParser
    (addPostToDatabase) where

import qualified Data.Text as T
import Control.Monad.Trans (liftIO)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Data.List.Split
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
        nonEmptyPartitions = if (null liPartitions) then numberedPartitions else liPartitions
        programPrereqs = map getCourseFromTag $ map (fromAttrib "href") $ filter isCourseTag programElements
        firstCourse = if (null programPrereqs) then Nothing else (Just (head programPrereqs))
    categoryParser requirements fullPostName firstCourse nonEmptyPartitions
    where
        isRequirementSection element = tagOpenAttrLit "div" ("class", "field-content") element
        isCourseTag tag = tagOpenAttrNameLit "a" "href" (\hrefValue -> T.isInfixOf "/course" hrefValue) tag
        isLiTag tag = isTagOpenName "li" tag

addPostCategoriesToDatabase :: [T.Text] -> SqlPersistM ()
addPostCategoriesToDatabase categories = do
    mapM_ addCategoryToDatabase (filter isCategory categories)
    where
        isCategory text =
            let infixes = map (containsText text)
                         ["First", "Second", "Third", "suitable", "Core", "Electives"]
            in
                ((T.length text) >= 7) && ((length $ filter (\bool -> bool) infixes) <= 0)
        containsText text subtext = T.isInfixOf subtext text

addCategoryToDatabase :: T.Text -> SqlPersistM ()
addCategoryToDatabase category =
    insert_ $ PostCategory category (T.pack "")

-- Helpers

categoryParser :: [Tag T.Text] -> T.Text -> Maybe T.Text -> [T.Text] -> SqlPersistM ()
categoryParser tags fullPostName firstCourse listPartitions = do
    case parsed of
        Right (post, categories) -> do
            postExist <- insertUnique post
            case postExist of
                Just _ -> do
                    addPostCategoriesToDatabase categories
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
getNumberedPartitions tags = do
    let pTags = partitions isPTag tags
        pTagsWithoutBr = map (splitWhen isBRTag) pTags
    concat pTagsWithoutBr
    where
        isPTag tag = isTagOpenName "p" tag
        isBRTag currentTag = isTagOpenName "br" currentTag
       

parseNumberedPartition :: [Tag T.Text] -> T.Text
parseNumberedPartition pPartition = do
    let parsed = P.parse parseNumberedLine failedString (innerText pPartition)
    case parsed of
        Right category ->  T.replace "\n" " " $ T.replace "\8203" " " $ T.replace "\160" " " $ T.strip category
        Left _ -> ""
