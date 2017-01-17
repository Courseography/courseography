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
import Text.Parsec.String (Parser)

getCourseFromTag :: String -> String
getCourseFromTag courseTag =
    let course = P.parse findCourseFromTag "(source)" courseTag
    in
        case course of
            Right name -> name
            Left _ -> ""

findCourseFromTag :: Parser String
findCourseFromTag = do
    parseUntil (P.char '#')
    P.many1 P.anyChar

generalCategoryParser :: Maybe String -> Parser (String, String, String, [String])
generalCategoryParser firstCourse = do
    (description, departmentName, postType) <- (postInfoParser firstCourse)
    categories <- splitPrereqText

    return (description, departmentName, postType, categories)

-- Post Parsing

postInfoParser :: Maybe String -> Parser (String, String, String)
postInfoParser firstCourse = do
    departmentName <- getDepartmentName
    postType <- getPostType
    description <- getRequirements firstCourse

    return (description, departmentName, postType)

extractPostType :: String -> String
extractPostType postCode = do
    let parsed = P.parse findPostType "(source)" postCode
    case parsed of
        Right name -> name
        Left _ -> ""

findPostType :: Parser String
findPostType = do
   P.string "AS"
   P.many1 P.letter

getDepartmentName :: Parser String
getDepartmentName = 
    (P.try (parseUntil ((P.try (P.lookAhead (P.string " Specialist"))) <|>
                        (P.try (P.lookAhead (P.string " Major"))) <|> 
                        (P.try (P.lookAhead (P.string " Minor"))))))   

getPostType :: Parser String 
getPostType = do
    P.spaces
    ((P.try (P.string "Specialist")) <|>
     (P.try (P.string "Major")) <|>
     (P.try (P.string "Minor")))

isDepartmentName ::  [Char] -> Parser String
isDepartmentName postType = parseUntil (P.string postType)


-- Post Category Parsing

getRequirements :: Maybe String -> Parser String
getRequirements firstCourse =
    (P.try (parseUntil (P.string "First Year"))) <|>
    (P.try (parseUntil (P.string "Program Course Requirements:"))) <|>
    (P.try (parseUntil (P.string "Program requirements:"))) <|>
    (findFirstCourse firstCourse)

findFirstCourse :: Maybe String -> Parser String
findFirstCourse firstCourse =
    case firstCourse of
        Nothing -> parseUntil P.eof
        Just course -> (P.try (parseUntil (P.lookAhead (P.string course)))) <|> (parseUntil P.eof)

parseNoteLine :: Parser String
parseNoteLine = do
    P.string "Note"
    (P.try (parseUntil (P.char '\n'))) <|> (parseUntil P.eof)

parseNotes :: Parser String
parseNotes = do
    (P.try (P.string "Notes")) <|> (P.try (P.string "NOTES"))
    parseUntil P.eof
    return ""

parseUntil :: Parser a -> Parser String
parseUntil parser = P.manyTill P.anyChar (P.try parser)

splitPrereqText :: Parser [String]
splitPrereqText = do
    P.manyTill ((P.try parseNotes) <|> (P.try parseNoteLine) <|>
        (P.try (parseCategory False)) <|> (parseUntil P.eof)) P.eof

parseCategory :: Bool -> Parser String
parseCategory withinBracket = do
    left <- parseUpToSeparator
    nextChar <- P.anyChar
    if nextChar == ',' && (not withinBracket)
    then return left
    else
        mergeText left nextChar withinBracket

mergeText :: String -> Char -> Bool -> Parser String
mergeText left nextChar withinBracket = do
    case nextChar of
        '(' -> do
            right <- P.option " " (parseCategory True)
            return $ "(" ++ left ++ right
        ')' -> do
            right <- P.option " " (parseCategory False)
            return $ left ++ ")" ++ right
        '/' -> do
            right <- P.option " " (parseCategory withinBracket)
            return $ left ++ " or " ++ right
        ',' -> do
            right <- P.option " " (parseCategory withinBracket)
            case withinBracket of
                True -> return $ left ++ " and " ++ right
                False -> return left
        _ -> return left

parseUpToSeparator :: Parser String
parseUpToSeparator = parseUntil (P.notFollowedBy (P.noneOf ",/();\r\n"))

-- For testing purposed in REPL
parseAll :: Parser [String]
parseAll = P.many (parseCategory False)

