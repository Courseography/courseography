module WebParsing.ArtSciParser
    (parseCalendar, getDeptList) where

import Config (fasCalendarUrl, programsUrl, runDb)
import Control.Monad.IO.Class (liftIO)
import Data.CSV
import Data.List (findIndex, nubBy)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Database.CourseInsertion (insertCourse)
import Database.Persist (insertUnique)
import Database.Persist.Sqlite (Filter, SqlPersistM, deleteWhere, insertMany_)
import Database.Tables (Building (..), Courses (..), Department (..))
import Filesystem.Path.CurrentOS as Path
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import System.Directory (getCurrentDirectory)
import qualified Text.HTML.TagSoup as TS
import Text.HTML.TagSoup (Tag)
import Text.HTML.TagSoup.Match (anyAttrValue, tagOpen, tagOpenAttrLit, tagOpenAttrNameLit)
import Text.Parsec (count, many, parse)
import qualified Text.Parsec.Char as P
import Text.Parsec.Text (Parser)
import Text.ParserCombinators.Parsec (parseFromFile)
import Util.Helpers
import WebParsing.ParsecCombinators (text)
import WebParsing.PostParser (addPostToDatabase)
import WebParsing.ReqParser (parseReqs)

parseCalendar :: IO ()
parseCalendar = do
    parseArtSci
    parseBuildings

-- The file name is building.csv and it is in the courseography/db folder
buildingsCSV :: IO Prelude.FilePath
buildingsCSV = do
    curDir <- getCurrentDirectory
    return $ Path.encodeString $ Path.append (Path.decodeString curDir) $ Path.append (Path.decodeString "db") (Path.decodeString "building.csv")

parseBuildings :: IO ()
parseBuildings = do
    buildingInfo <- getBuildingsFromCSV =<< buildingsCSV
    runDb $ do
        liftIO $ putStrLn "Inserting buildings"
        deleteWhere ([] :: [Filter Building])  :: SqlPersistM ()
        insertMany_ buildingInfo :: SqlPersistM ()

-- | Extract building names, codes, addresses, postal codes, latitude and longitude from csv file
getBuildingsFromCSV :: String -> IO [Building]
getBuildingsFromCSV buildingCSVFile = do
    buildingCSVData <- parseFromFile csvFile buildingCSVFile
    case buildingCSVData of
        Left _ -> error "csv parse error"
        Right buildingData -> do
            return $ map (\b -> Building (T.pack $ safeHead "" b)
                                        (T.pack (b !! 1))
                                        (T.pack (b !! 2))
                                        (T.pack (b !! 3))
                                        (read (b !! 4) :: Double)
                                        (read (b !! 5) :: Double)) $ drop 1 buildingData

-- | Parses the entire Arts & Science Course Calendar and inserts courses
-- into the database.
parseArtSci :: IO ()
parseArtSci = do
    programs <- programsUrl
    bodyTags <- httpBodyTags programs
    let deptInfo = getDeptList bodyTags
    runDb $ do
        liftIO $ putStrLn "Inserting departments"
        insertDepts $ map snd deptInfo
        mapM_ parseDepartment (nubBy (\(x, _) (y, _) -> x == y) deptInfo)

-- | Converts the processed main page and extracts a list of department html pages
-- and department names
getDeptList :: [Tag T.Text] -> [(T.Text, T.Text)]
getDeptList tags =
    let tables = TS.partitions (TS.isTagOpenName "table") tags  -- every partition is a table
        tables' = map (takeWhile (not . TS.isTagCloseName "table")) tables
        depts = concatMap extractDepartments tables'
    in  depts
    where
        extractDepartments :: [Tag T.Text] -> [(T.Text, T.Text)]
        extractDepartments tableTags =
            -- Each aTag consists of a start tag, text, and end tag
            let aTags = TS.partitions (tagOpenAttrNameLit "a" "href" (const True)) tableTags
                depts = mapMaybe getDept aTags
                getDept tag =
                    case tag of
                        [] -> Nothing
                        (x:xs) -> Just (TS.fromAttrib "href" x, T.strip $ TS.innerText (x:xs))
            in
                filter (\(a, b) -> not (T.null a) && not (T.null b)) depts

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
parseCourses :: [Tag T.Text] -> [(Courses, T.Text, T.Text)]
parseCourses tags =
    let elems = TS.partitions isAccordion tags
        courses = map parseCourse elems
    in
        courses
    where
        isAccordion = tagOpenAttrNameLit "h3" "class" (T.isInfixOf "js-views-accordion-group-header")

        parseCourse :: [Tag T.Text] -> (Courses, T.Text, T.Text)
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
                (Courses code
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
cleanText = T.replace "\n" "" . T.replace "\8203" "" . T.replace "\160" "" . T.strip
