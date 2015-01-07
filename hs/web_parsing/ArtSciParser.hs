import Network.HTTP
import Text.HTML.TagSoup
import Data.List
import Data.List.Utils

fasCalendarURL :: String
fasCalendarURL = "http://www.artsandscience.utoronto.ca/ofr/calendar/"

getDeptList :: [Tag String] -> [String]
getDeptList tags =
    let lists = sections (~== TagOpen "ul" [("class","simple")]) tags
        contents = takeWhile (~/= TagClose "ul") . head . tail $ lists
        as = filter (~== TagOpen "a" []) contents
    in  map (\x -> case x of TagOpen "a" [("href", link)] -> link) as

doStuff :: String -> [String]
doStuff s = 
    let tags = parseTags s
    in  getDeptList tags

getCalendar :: [String] -> IO ()
getCalendar strs = do
    let path = fasCalendarURL ++ head strs
    rsp <- simpleHTTP (getRequest path)
              -- fetch document and return it (as a 'String'.)
    body <- getResponseBody rsp
    let tags = filter isComment $ parseTags body
    let coursesSoup = lastH2 tags
    let courses = map (filter (~== TagText "")) $ partitions isCourseTitle coursesSoup
    let course = map (processCourse . filter (~/= TagText "\r\n")) courses
    print course
    where
        isComment (TagComment _) = False
        isComment _ = True

        lastH2 = last . sections (~== TagOpen "h2" [])

        isCourseTitle (TagOpen _ attrs) = any (\x -> fst x == "name" && length (snd x) == 8) attrs
        isCourseTitle _ = False

processCourse :: [Tag String] -> [String]
processCourse tags =
    let cleanTags = map cleanTag tags
        title = find (~== TagText "") cleanTags
        titleText = getTitleText title
    in --[titleText]
        map getText cleanTags
    where
        getTitleText (Just (TagText s)) = replace "\160\160\160\160" " " s

        cleanTag (TagText s) = TagText (replace "\r\n                   " " " s)

        getText (TagText s) = s


main :: IO ()
main = do
    rsp <- simpleHTTP (getRequest fasCalendarURL)
              -- fetch document and return it (as a 'String'.)
    body <- getResponseBody rsp
    let depts = doStuff body
    getCalendar $ filter (== "crs_csc.htm") depts
