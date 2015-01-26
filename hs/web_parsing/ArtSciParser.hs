import Network.HTTP
import Text.HTML.TagSoup
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as B
import Data.List.Utils
import Tables

{----------------------------------------------------------------------------------------
TODO
    use Data.Text type instead of String for all functions
    remove duplicates
----------------------------------------------------------------------------------------}

fasCalendarURL :: String
fasCalendarURL = "http://www.artsandscience.utoronto.ca/ofr/calendar/"

{-
A collection of pages that do not contain any course information
-}
toDelete :: [String]
toDelete = ["199299398399Big_Ideas_(Faculty_of_Arts_&_Science_Programs).html",
            "Joint_Courses.html",
            "Writing_in_the_Faculty_of_Arts_&_Science.html"]

{----------------------------------------------------------------------------------------
INPUT: 'processed' main page
OUTPUT: a list of department html page names
-----------------------------------------------------------------------------------------}
getDeptList :: [Tag String] -> [String]
getDeptList tags =
    let lists = sections (~== TagOpen "ul" [("class","simple")]) tags
        contents = takeWhile (~/= TagClose "ul") . head . tail $ lists
        as = filter (~== TagOpen "a" []) contents
        rawList = map (\x -> case x of TagOpen "a" [("href", link)] -> link) as
    in nub $ filter (\dept -> not (dept `elem` toDelete)) rawList

{----------------------------------------------------------------------------------------
INPUT: a string representation of the ArtSci html page
OUTPUT: a list of department html page names

a simple 'preprocessing' function
----------------------------------------------------------------------------------------}
doStuff :: String -> [String]
doStuff s = 
    let tags = parseTags s
    in  getDeptList tags

{----------------------------------------------------------------------------------------
INPUT: an html filename of a department (which are found from getDeptList)
OUTPUT: a list, where each element is a list of strings and tags relating to a single
course found in that department
----------------------------------------------------------------------------------------}
getCalendar :: String -> IO ()
getCalendar str = do
    let path = fasCalendarURL ++ str
    rsp <- simpleHTTP (getRequest path)
              -- fetch document and return it (as a 'String'.)
    body <- getResponseBody rsp
    let tags = filter isComment $ parseTags body
    --course description begins after the final h2 heading
    let coursesSoup = lastH2 tags
    --partitions tags into sections, where each element starts with course name
    let courses = map (filter (~== TagText "")) $ partitions isCourseTitle coursesSoup
    --converts course partitions into course Records
    let course = map (processCourseToData . filter (~/= TagText "\r\n")) courses
    mapM_ (B.putStrLn . name)  course
    --print  course
    where
        isComment (TagComment _) = False
        isComment _ = True

        lastH2 = last . sections (~== TagOpen "h2" [])

        isCourseTitle (TagOpen _ attrs) = any (\x -> fst x == "name" && length (snd x) == 8) attrs
        isCourseTitle _ = False

{----------------------------------------------------------------------------------------
INPUT: a list of tags representing a single course, 
OUTPUT: Course 'record' containing course info
----------------------------------------------------------------------------------------}
processCourseToData :: [Tag String] -> Course
processCourseToData tags  =
    let cleanTags = map cleanTag tags
        getTitle = find (~== TagText "") cleanTags
        --splitat 8 since first 8 represent course code
        courseNames = splitAt 8 $ removeTitleGarbage $ removeLectureSection $ getTitle
    in Course {breadth = Nothing, 
            description = Nothing, 
            --we drop 1 to remove space between title and course 
            title  = (Just (T.pack $ drop 1 $ snd courseNames)),
            prereqString = Nothing,
            f = Nothing,
            s = Nothing,
            y = Nothing,
            name = T.pack $ fst courseNames,
            exclusions = Nothing,
            manualTutorialEnrol = Nothing ,
            distribution = Nothing,
            prereqs = Nothing
        }
    where
        --remove [12l/24t] or similar from tag containing course code and name
        removeLectureSection (Just (TagText s)) = takeWhile (/= '[') s

        removeTitleGarbage s = replace "\160\160\160\160" " " s

        cleanTag (TagText s) = TagText (replace "\r\n                   " " " s)

main :: IO ()
main = do
    rsp <- simpleHTTP (getRequest fasCalendarURL)
              -- fetch document and return it (as a 'String'.)
    body <- getResponseBody rsp
    let depts = doStuff body
    --getCalendar $ filter (== "crs_csc.htm") depts
    mapM_ getCalendar depts


{-
doStuff -> getDeptList -> map getCalendar 
-}

{----------------------------------------------------------------------------------------
course codes can be found in body <a href="csc_...#asdf">
only other links either contain utoronto, or maps.google in href property
----------------------------------------------------------------------------------------}
{-
getCrossListedCourses :: IO()
getCrossListedCourses =  do
    let path = fasCalendarURL ++ "Joint_Courses.html"
    rsp <- simpleHTTP (getRequest path)
              -- fetch document and return it (as a 'String'.)
    body <- getResponseBody rsp
    let tags  = partitions (isTagOpenName "a") (parseTags body)
    let names = map (takeWhile (\x -> not (isTagClose x))) tags
    let removeUseless = filter (\x -> not (isOtherLink (head x))) names
    mapM_ (fromTagText . head . tail) removeUseless
    where
        isOtherLink (TagOpen _ [("href", link)]) = isInfixOf ".ca" link
        isOtherLink (TagOpen _ [("name", _)]) = True
        isotherLink _ = False
-}

{----------------------------------------------------------------------------------------
INPUT: a list of tags representing a single course
OUTPUT: 
----------------------------------------------------------------------------------------}
processCourse :: [Tag String] -> String
processCourse tags =
    let cleanTags = map cleanTag tags
        title = find (~== TagText "") cleanTags
        titleText = getTitleText title
    in --[titleText]
        --map getText cleanTags
        titleText
    where
        getTitleText (Just (TagText s)) = replace "\160\160\160\160" " " s
        cleanTag (TagText s) = TagText (replace "\r\n                   " " " s)
        getText (TagText s) = s
