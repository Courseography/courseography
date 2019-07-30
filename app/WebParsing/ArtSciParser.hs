module WebParsing.ArtSciParser
    (parseArtSci, getDeptList, parseBuildings) where

import Data.Either (either)
import Data.List (elemIndex, nubBy)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Text.Parsec (count, many, parse, (<|>))
import Text.Parsec.Text (Parser)
import qualified Text.Parsec.Char as P
import WebParsing.ParsecCombinators (text)
import Network.HTTP.Simple (parseRequest, getResponseBody, httpLBS)
import qualified Text.HTML.TagSoup as TS
import Text.HTML.TagSoup (Tag)
import Database.Persist.Sqlite (runSqlite, SqlPersistM, insertMany_)
import Database.Persist (insertUnique)
import Database.CourseInsertion (insertCourse)
import Database.Tables (Courses(..), Department(..), Building(..))
import WebParsing.ReqParser (parseReqs)
import Config (databasePath)
import WebParsing.PostParser (addPostToDatabase)
import Data.CSV
import Text.ParserCombinators.Parsec (parseFromFile)
import Text.HTML.TagSoup.Match (tagOpenAttrLit, tagOpenAttrNameLit)
import System.Directory (getCurrentDirectory)
import Filesystem.Path.CurrentOS as Path
import Config (fasCalendarUrl, programsUrl)

-- The file name is building.csv and it is in the courseography/db folder
buildingsCSV :: IO Prelude.FilePath
buildingsCSV = do
    curDir <- getCurrentDirectory
    return $ Path.encodeString $ Path.append (Path.decodeString curDir) $ Path.append (Path.decodeString "db") (Path.decodeString "building.csv")

parseBuildings :: IO ()
parseBuildings = do
    buildingInfo <- getBuildingsFromCSV =<< buildingsCSV
    runSqlite databasePath $ do
        liftIO $ putStrLn "Inserting buildings"
        insertMany_ buildingInfo :: SqlPersistM ()

-- | Extract building names, codes, addresses, postal codes, latitude and longitude from csv file
getBuildingsFromCSV :: String -> IO [Building]
getBuildingsFromCSV buildingCSVFile = do
    buildingCSVData <- parseFromFile csvFile buildingCSVFile
    case buildingCSVData of
        Left _ -> error "csv parse error"
        Right buildingData -> do
            return $ map (\b -> Building (T.pack (b !! 0))
                                        (T.pack (b !! 1))
                                        (T.pack (b !! 2))
                                        (T.pack (b !! 3))
                                        (read (b !! 4) :: Double)
                                        (read (b !! 5) :: Double)) $ drop 1 buildingData

-- | Parses the entire Arts & Science Course Calendar and inserts courses
-- into the database.
parseArtSci :: IO ()
parseArtSci = do
    bodyTags <- httpBodyTags programsUrl
    let deptInfo = getDeptList bodyTags
    runSqlite databasePath $ do
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
                depts = map (\t -> (TS.fromAttrib "href" $ head t, T.strip $ TS.innerText t)) aTags
            in
                filter (\(a, b) -> (not $ T.null a) && (not $ T.null b)) depts

-- | Insert department names to database
insertDepts :: [T.Text] -> SqlPersistM ()
insertDepts = mapM_ (print >> (insertUnique . Department))

-- | Takes the URL and name of a department name for parsing.
parseDepartment :: (T.Text, T.Text) -> SqlPersistM ()
parseDepartment (relativeURL, _) = do
    bodyTags <- liftIO $ httpBodyTags $ fasCalendarUrl ++ T.unpack relativeURL
    let contentTags = dropWhile (not . tagOpenAttrLit "div" ("id", "block-system-main")) bodyTags
        contentTags' = takeWhile (not . tagOpenAttrLit "p" ("class", "rteright")) contentTags
        programs = dropWhile (not . tagOpenAttrNameLit "div" "class" isProgramHeaderInfix) contentTags'
        programs' = takeWhile (not . tagOpenAttrNameLit "div" "class" (T.isInfixOf "view-id-course_group_view")) programs
        courseTags = dropWhile (not . tagOpenAttrNameLit "div" "class" isCourseSection) contentTags'
    parsePrograms programs'
    mapM_ insertCourse $ parseCourses courseTags
    where
        isProgramHeaderInfix tag = or [(T.isInfixOf "view-id-section") tag, (T.isInfixOf "view-header") tag]
        isCourseSection tag = or [(T.isInfixOf "view-id-courses") tag,
            and [(T.isInfixOf "view-") tag, (T.isInfixOf "-courses") tag,
                  not (T.isInfixOf "programs" tag)]]

-- | Parse the section of the course calendar listing the programs offered by a department.
parsePrograms :: [Tag T.Text] -> SqlPersistM ()
parsePrograms programs = do
    let elems = TS.partitions isPost programs
    mapM_ addPostToDatabase elems
    where
         isPost tag = tagOpenAttrNameLit "h3" "class" isProgramsView tag
         isProgramsView currentTag = or [(T.isInfixOf "programs_view") currentTag, (T.isInfixOf "_programs") currentTag]

-- | Parse the section of the course calendar listing the courses offered by a department.
parseCourses :: [Tag T.Text] -> [(Courses, T.Text, T.Text)]
parseCourses tags =
    let elems = TS.partitions (tagOpenAttrNameLit "h3" "class" (T.isInfixOf "views-accordion")) tags
        courses = map parseCourse elems
    in
        courses
    where
        parseCourse :: [Tag T.Text] -> (Courses, T.Text, T.Text)
        parseCourse courseTags =
            let courseHeader = T.strip . TS.innerText $ takeWhile (not . TS.isTagCloseName "h3") courseTags
                (code, title) = either (error . show) id $ parse parseCourseTitle "course title" courseHeader
                spans = TS.partitions (TS.isTagOpenName "span") courseTags
                courseContents = map (T.strip . TS.innerText) spans
                i1 = elemIndex "Hours:" courseContents
                -- TODO: add the number of contact hours to the database
                (_, description) = maybe ("", "") (\i -> either (error . show) id $ parse parseHours "course hours" $ courseContents !! (i+1)) i1
                prereqString = fmap ((courseContents!!) . (+1)) $ elemIndex "Prerequisite:" courseContents
                coreq = fmap ((courseContents!!) . (+1)) $ elemIndex "Corequisite:" courseContents
                -- TODO: add a "recommended preparation" field to the database
                -- prep = maybe "" ((courseContents!!) . (+1)) $ elemIndex "Recommended Preparation:" courseContents
                exclusion = fmap ((courseContents!!) . (+1)) $ elemIndex "Exclusion:" courseContents
                distribution = maybe "" ((courseContents!!) . (+1)) $ elemIndex "Distribution Requirements:" courseContents
                breadth = maybe "" ((courseContents!!) . (+1)) $ elemIndex "Breadth Requirements:" courseContents
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

-- | Parse a course's code and title.
parseCourseTitle :: Parser (T.Text, T.Text)
parseCourseTitle = do
    dept <- count 3 P.letter
    num <- count 3 P.digit
    session <- P.letter
    campus <- P.digit
    _ <- text " - "
    title <- many P.anyChar
    return (T.pack $ dept ++ num ++ [session, campus], T.pack title)

-- | Parse a course's number of contact hours and description.
parseHours :: Parser (T.Text, T.Text)
parseHours = do
    hours <- many (P.alphaNum <|> P.char '/')
    _ <- many P.space
    description <- many P.anyChar
    return (T.pack hours, T.pack description)

-- | Make an HTTP(S) request and convert the body into a list of Tags.
httpBodyTags :: String -> IO [Tag T.Text]
httpBodyTags url = do
    req <- parseRequest url
    response <- httpLBS req
    return . TS.parseTags . cleanText . toStrict . decodeUtf8 . getResponseBody $ response

-- | Remove odd characters from text
cleanText :: T.Text -> T.Text
cleanText = T.replace "\n" "" . T.replace "\8203" "" . T.replace "\160" "" . T.strip
