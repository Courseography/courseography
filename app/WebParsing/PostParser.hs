module WebParsing.PostParser
    (addPostToDatabase) where

import Network.HTTP
import qualified Data.Text as T
import Control.Monad.Trans (liftIO)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Config (databasePath)
import Database.Tables
import Database.Persist.Sqlite (insert_, runMigration, runSqlite, SqlPersistM)
import Database.Persist (insertUnique)
import qualified Text.Parsec as P
import WebParsing.ParsecCombinators (getCourseFromTag, generalCategoryParser, parseCategory,
    postInfoParser)

fasCalendarURL :: String
fasCalendarURL = "http://calendar.artsci.utoronto.ca/"

failedString :: String
failedString = "Failed."

--getPost :: T.Text -> IO ()
--getPost str = do
--    let path = fasCalendarURL ++ (T.unpack str)
--    rsp <- simpleHTTP (getRequest path)
--    body <- getResponseBody rsp
--    let tags = filter isNotComment $ parseTags body
--        postsSoup = secondH2 tags
--        posts = partitions isPostName postsSoup
--    runSqlite databasePath $ do
--        runMigration migrateAll
--        mapM_ addPostToDatabase posts
--    print $ "parsing " ++ (T.unpack str)
--    where
--        isNotComment (TagComment _) = False
--        isNotComment _ = True
--        secondH2 tags =
--            let sect = sections (isTagOpenName "h2") tags
--            in
--                if (length sect) < 2
--                then
--                    []
--                else
--                    takeWhile isNotCoursesSection tags
--        isNotCoursesSection tag = not (tagOpenAttrLit "a" ("name", "courses") tag)
--        isPostName tag = tagOpenAttrNameLit "a" "name" (\nameValue -> (length nameValue) == 9) tag

addPostToDatabase :: [Tag T.Text] -> SqlPersistM ()
addPostToDatabase programElements = do
    --let postCode_ = T.pack (fromAttrib "name" ((take 1 $ filter (isTagOpenName "a") tags) !! 0))
    --    liPartitions = partitions isLiTag tags
    --    programPrereqs = map getCourseFromTag $ map (T.pack . fromAttrib "href") $ filter isCourseTag tags
    --    firstCourse = if (null programPrereqs) then Nothing else (Just (head programPrereqs))
    --categoryParser tags firstCourse postCode_ liPartitions
    --where
    --    isCourseTag tag = tagOpenAttrNameLit "a" "href" (\hrefValue -> (length hrefValue) >= 0) tag
    --    isLiTag tag = isTagOpenName "li" tag

    -- TODO: Remove Focuses from programElements
    -- TODDO: Store name of post before we lose that information in the next line
    let fullPostName = innerText $ take 1 $ filter isTagText programElements
        requirements = last $ sections isRequirementSection programElements
        liPartitions = partitions isLiTag requirements
        programPrereqs = map getCourseFromTag $ map (fromAttrib "href") $ filter isCourseTag programElements
        firstCourse = if (null programPrereqs) then Nothing else (Just (head programPrereqs))
    categoryParser requirements fullPostName firstCourse liPartitions
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

categoryParser :: [Tag T.Text] -> T.Text -> Maybe T.Text -> [[Tag T.Text]] -> SqlPersistM ()
categoryParser tags fullPostName firstCourse liPartitions = do
    case parsed of
        Right (post, categories) -> do
            postExist <- insertUnique post
            case postExist of
                Just _ -> do
                    liftIO (print categories)
                    addPostCategoriesToDatabase categories
                Nothing -> return ()
        Left err -> do
            --liftIO $ print failedString
            liftIO $ print err
            return ()
    where
        parsed = case liPartitions of
            [] -> P.parse (generalCategoryParser fullPostName firstCourse) failedString (innerText tags)
            partitionResults -> do
                let categories = map parseLi partitionResults
                post <- P.parse (postInfoParser fullPostName firstCourse) failedString (innerText tags)
                return (post, categories)

parseLi :: [Tag T.Text] -> T.Text
parseLi liPartition = do
    let parsed = P.parse parseCategory failedString (innerText liPartition)
    case parsed of
        Right category -> category
        Left _ -> ""
