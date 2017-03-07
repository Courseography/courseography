{-# LANGUAGE OverloadedStrings #-}
module WebParsing.ParsecCombinators
    (getCourseFromTag,
     findCourseFromTag,
     getPostType,
     getDepartmentName,
     isDepartmentName,
     generalCategoryParser,
     parseCategory,
     postInfoParser) where

import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import qualified Data.Text as T
import Text.Parsec.Text (Parser)
import Database.Tables
import Control.Monad (mapM)

getCourseFromTag :: T.Text -> T.Text
getCourseFromTag courseTag =
    let course = P.parse findCourseFromTag "(source)" courseTag
    in
        case course of
            Right name -> name
            Left _ -> ""

findCourseFromTag :: Parser T.Text
findCourseFromTag = do
    _ <- parseUntil (P.char '#')
    parsed <- P.many1 P.anyChar
    return $ T.pack parsed

generalCategoryParser :: Maybe T.Text -> T.Text -> Parser (Post, [T.Text])
generalCategoryParser firstCourse postCode = do
    post <- postInfoParser firstCourse postCode
    categories <- splitPrereqText

    return (post, categories)

-- Post Parsing

postInfoParser :: Maybe T.Text -> T.Text -> Parser Post
postInfoParser firstCourse postCode = do
    departmentName <- getDepartmentName
    postType <- getPostType
    description <- getRequirements firstCourse

    return $ Post postType departmentName postCode description

extractPostType :: T.Text -> T.Text
extractPostType postCode = do
    let parsed = P.parse findPostType "(source)" postCode
    case parsed of
        Right name -> name
        Left _ -> ""

findPostType :: Parser T.Text
findPostType = do
   _ <- text "AS"
   parsed <- P.many1 P.letter
   return $ T.pack parsed

getDepartmentName :: Parser T.Text
getDepartmentName =
    P.try (parseUntil (P.try (P.lookAhead (text " Specialist")) <|>
                        P.try (P.lookAhead (text " Major")) <|>
                        P.try (P.lookAhead (text " Minor"))))

getPostType :: Parser T.Text
getPostType = do
    _ <- P.spaces
    (P.try (text "Specialist") <|>
     P.try (text "Major") <|>
     P.try (text "Minor"))

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
    nextChar <- P.anyChar
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
