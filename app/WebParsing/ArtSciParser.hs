module WebParsing.ArtSciParser
    (parseCalendar, parseDepartmentList) where

import Config (fasCalendarUrl, programsUrl, runDb)
import Control.Monad.IO.Class (liftIO)
import Data.List (findIndex, nubBy)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Bifunctor as BF
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Database.Persist (insertUnique)
import Database.Persist.Sqlite (SqlPersistM)
import Database.Tables (Course (..), Department (..))
import Models.Building (parseBuildings)
import Models.Course (insertCourse)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import qualified Text.HTML.TagSoup as TS
import Text.HTML.TagSoup (Tag)
import Text.HTML.TagSoup.Match (anyAttrValue, tagOpen, tagOpenAttrLit, tagOpenAttrNameLit)
import Text.Parsec (count, many, parse)
import qualified Text.Parsec.Char as P
import Text.Parsec.Text (Parser)
import WebParsing.ParsecCombinators (text)
import WebParsing.PostParser (addPostToDatabase)
import WebParsing.ReqParser (parseReqs)

parseCalendar :: IO ()
parseCalendar = do
    parseArtSci
    parseBuildings

-- | Parses the entire Arts & Science Course Calendar and inserts courses
-- into the database.
parseArtSci :: IO ()
parseArtSci = do
    programs <- programsUrl
    deptInfo <- parseDepartmentList programs
    runDb $ do
        liftIO $ putStrLn "Inserting departments"
        insertDepts $ map snd deptInfo
        mapM_ parseDepartment (nubBy (\(x, _) (y, _) -> x == y) deptInfo)

-- | Parse the list of all departments, given the URL of the program/subject areas page. 
-- Exclude departments with no courses, duplicate courses, and program areas belonging to a college.
parseDepartmentList :: String -> IO [(T.Text, T.Text)]
parseDepartmentList url = do
    let ignoredDepts = ["ASIP (Arts & Science Internship Program)",
                        "Biology",
                        "Combined Degree Programs",
                        "Data Science",
                        "Faculty of Arts and Science Programs (299/398/399)",
                        "Laboratory Medicine and Pathobiology)", -- | Displayed as "Pathobiology (see Laboratory Medicine and Pathobiology)" on program areas page
                        "Research Opportunity/Research Excursions (299/398/399)"]
    bodyTags <- httpBodyTags url
    let deptList = getDeptList bodyTags
    return $ filter (isValidDepartment ignoredDepts) deptList
    where
        isValidDepartment :: [T.Text] -> (T.Text, T.Text) -> Bool
        isValidDepartment ignoredDepts (deptPage, deptName) = 
            "/" `T.isPrefixOf` deptPage && 
            deptName `notElem` ignoredDepts && 
            not (" College)" `T.isSuffixOf` deptName)

-- | Converts the processed main page and extracts a list of department html pages
-- and department names
getDeptList :: [Tag T.Text] -> [(T.Text, T.Text)]
getDeptList tags =
    let tables = TS.partitions (TS.isTagOpenName "table") tags  -- every partition is a table
        tables' = map (takeWhile (not . TS.isTagCloseName "table")) tables
        depts = concatMap extractDepartments tables'
    in map (BF.second cleanText) depts
    where
        extractDepartments :: [Tag T.Text] -> [(T.Text, T.Text)]
        extractDepartments tableTags =
            -- Each aTag consists of a start tag, text, and end tag
            let aTags = TS.partitions (tagOpenAttrNameLit "a" "href" (const True)) tableTags
                depts = mapMaybe getDept aTags
            in
                filter (\(a, b) -> not (T.null a) && not (T.null b)) depts
            where
                getDept :: [Tag T.Text] -> Maybe (T.Text, T.Text)
                getDept [] = Nothing
                getDept (x:xs) = Just (TS.fromAttrib "href" x, T.strip $ TS.innerText (x:xs))

-- | Insert department names to database
insertDepts :: [T.Text] -> SqlPersistM ()
insertDepts = mapM_ (print >> (insertUnique . Department))

-- | Takes the URL and name of a department name for parsing.
parseDepartment :: (T.Text, T.Text) -> SqlPersistM ()
parseDepartment (relativeURL, _) = do
    liftIO $ print relativeURL
    fasCalendar <- liftIO fasCalendarUrl
    bodyTags <- liftIO $ httpBodyTags $ fasCalendar ++ T.unpack relativeURL
    let contentTags = dropWhile (not . tagOpenAttrLit "footer" ("class", "view-footer")) bodyTags
        programs = dropWhile (not . tagOpenAttrNameLit "div" "class" isProgramHeaderInfix) contentTags
        programs' = dropWhile (not . tagOpenAttrNameLit "div" "class" (T.isInfixOf "view-content")) programs
        courseTags = dropWhile (not . tagOpenAttrNameLit "div" "class" (T.isInfixOf "courses-view")) contentTags
        courseTags' = dropWhile (not . tagOpenAttrNameLit "div" "class" (T.isInfixOf "view-content")) courseTags
    parsePrograms programs'
    mapM_ insertCourse $ parseCourses courseTags'
    where
        isProgramHeaderInfix = T.isInfixOf "view-programs-view"

-- | Parse the section of the course calendar listing the programs offered by a department.
parsePrograms :: [Tag T.Text] -> SqlPersistM ()
parsePrograms programs = mapM_ addPostToDatabase $ TS.partitions isAccordionHeader programs
    where
        isAccordionHeader = tagOpenAttrNameLit "h3" "class" (T.isInfixOf "js-views-accordion-group-header")

-- | Parse the section of the course calendar listing the courses offered by a department.
parseCourses :: [Tag T.Text] -> [(Course, T.Text, T.Text)]
parseCourses tags =
    let elems = TS.partitions isAccordion tags
        courses = map parseCourse elems
    in
        courses
    where
        isAccordion = tagOpenAttrNameLit "h3" "class" (T.isInfixOf "js-views-accordion-group-header")

        parseCourse :: [Tag T.Text] -> (Course, T.Text, T.Text)
        parseCourse courseTags =
            let courseHeader = T.strip . TS.innerText $ takeWhile (not . TS.isTagCloseName "h3") courseTags
                (code, title) = either (error . show) id $ parse parseCourseTitle "course title" courseHeader
                spans = TS.partitions (tagOpen (const True) (anyAttrValue $ T.isInfixOf "views-field")) courseTags
                courseContents = map (T.strip . TS.innerText) spans
                i1 = findIndex (T.isPrefixOf "Hours:") courseContents
                -- TODO: add the number of contact hours to the database
                -- hours = getValue "Hours:" courseContents
                description = maybe "" ((courseContents!!) . (+1)) i1
                prereqString = getValue "Prerequisite:" courseContents
                coreq = getValue "Corequisite:" courseContents
                -- TODO: add a "recommended preparation" field to the database
                -- prep = getValue "Recommended Preparation:" courseContents
                exclusion = getValue "Exclusion:" courseContents
                distribution = fromMaybe "" $ getValue "Distribution Requirements:" courseContents
                breadth = fromMaybe "" $ getValue "Breadth Requirements:" courseContents
            in
                (Course code
                    (Just title)
                    (Just description)
                    (fmap (T.pack . show . parseReqs . T.unpack) prereqString)
                    exclusion
                    Nothing
                    Nothing
                    prereqString
                    coreq
                    [],
                breadth, distribution)

        getValue label texts = do
            i <- findIndex (T.isPrefixOf label) texts
            let contents = texts !! i
            val <- T.stripPrefix label contents
            return $ T.strip val

-- | Parse a course's code and title.
parseCourseTitle :: Parser (T.Text, T.Text)
parseCourseTitle = do
    dept <- count 3 P.letter
    num <- count 3 P.digit
    session <- P.letter
    campus <- P.digit
    _ <- many P.space
    _ <- text "-"
    _ <- many P.space
    title <- many P.anyChar
    return (T.pack $ dept ++ num ++ [session, campus], T.pack title)

-- | Make an HTTP(S) request and convert the body into a list of Tags.
httpBodyTags :: String -> IO [Tag T.Text]
httpBodyTags url = do
    req <- parseRequest url
    response <- httpLBS req
    return . TS.parseTags . cleanText . toStrict . decodeUtf8 . getResponseBody $ response

-- | Remove odd characters from text
cleanText :: T.Text -> T.Text
cleanText = T.replace "\n" "" . T.replace "\8203" "" . T.replace "\160" " " . T.strip
