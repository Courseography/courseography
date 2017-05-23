module WebParsing.ParsecCombinators
    (getCourseFromTag,
     findCourseFromTag,
     getPostType,
     getDepartmentName,
     isDepartmentName,
     generalCategoryParser,
     parseCategory,
     postInfoParser,
     text, parseAll) where

import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import qualified Data.Text as T
import Text.Parsec.Text (Parser)
import Database.Tables (Post(Post))
import Control.Monad (mapM)
import Database.DataType

getCourseFromTag :: T.Text -> T.Text
getCourseFromTag courseTag =
    let course = P.parse findCourseFromTag "(source)" courseTag
    in
        case course of
            Right courseName -> courseName
            Left _ -> ""

findCourseFromTag :: Parser T.Text
findCourseFromTag = do
    _ <- P.string "/course/"
    parsed <- P.many1 P.anyChar
    return $ T.pack parsed

generalCategoryParser :: T.Text -> Maybe T.Text -> Parser (Post, [T.Text])
generalCategoryParser fullPostName firstCourse = do
    post <- postInfoParser fullPostName firstCourse
    categories <- splitPrereqText
    return (post, categories)

-- Post Parsing
postInfoParser :: T.Text -> Maybe T.Text -> Parser Post
postInfoParser fullPostName firstCourse = do
    let parsed = P.parse getDeptNameAndPostType "(source)" fullPostName
    case parsed of
        Right (deptName, postType) -> do
            programDescription <- getRequirements firstCourse
            return $ Post (read $ T.unpack postType) deptName (T.pack " ") programDescription
        Left _ -> return $ Post Other (fullPostName) (T.pack " ") (T.pack " ")

getDeptNameAndPostType :: Parser (T.Text, T.Text)
getDeptNameAndPostType = do
    _ <- P.spaces
    deptName <- getDepartmentName
    postType <- getPostType
    return $ (deptName, postType)

getDepartmentName :: Parser T.Text
getDepartmentName =
    P.try (parseUntil (P.try (P.lookAhead (text " Specialist")) <|>
                        P.try (P.lookAhead (text " Major")) <|>
                        P.try (P.lookAhead (text " Minor"))))

getPostType :: Parser T.Text
getPostType = do
    _ <- P.spaces
    P.choice [P.try (text "Specialist"), P.try (text "Major"), P.try (text "Minor")]

isDepartmentName ::  T.Text -> Parser T.Text
isDepartmentName postType = parseUntil (text postType)


-- Post Category Parsing
getRequirements :: Maybe T.Text -> Parser T.Text
getRequirements firstCourse =
    P.try (parseUntil (text "First Year")) <|>
    P.try (parseUntil (text "Program Course Requirements:")) <|>
    P.try (parseUntil (text "Program requirements:")) <|>
    findFirstCourse firstCourse

findFirstCourse :: Maybe T.Text -> Parser T.Text
findFirstCourse firstCourse =
    case firstCourse of
        Nothing -> parseUntil P.eof
        Just course -> P.try (parseUntil (P.lookAhead (text course))) <|> parseUntil P.eof

parseNoteLine :: Parser T.Text
parseNoteLine = do
    _ <- P.string "Note"
    P.try (parseUntil (P.char '\n')) <|> parseUntil P.eof

parseNotes :: Parser T.Text
parseNotes = do
    _ <- P.try (text "Notes") <|> P.try (text "NOTES")
    _ <- parseUntil P.eof
    return ""

parseUntil :: Parser a -> Parser T.Text
parseUntil parser = do
    parsed <- P.manyTill P.anyChar (P.try parser)
    return $ T.pack parsed

splitPrereqText :: Parser [T.Text]
splitPrereqText = do
    P.manyTill (P.try parseNotes <|> P.try parseNoteLine <|>
        P.try parseCategory <|> parseUntil P.eof) P.eof

parseCategory :: Parser T.Text
parseCategory = do
    left <- parseUpToSeparator
    _ <- P.anyChar
    return left

parseUpToSeparator :: Parser T.Text
parseUpToSeparator = parseUntil (P.notFollowedBy (P.noneOf ";\r\n"))

text :: T.Text -> Parser T.Text
text someText = do
    parsed <- mapM P.char (T.unpack someText)
    return $ T.pack parsed

-- For testing purposed in REPL
parseAll :: Parser [T.Text]
parseAll = P.many parseCategory
