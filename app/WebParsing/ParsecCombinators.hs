{-# LANGUAGE OverloadedStrings #-}
module WebParsing.ParsecCombinators
    (getCourseFromTag,
     findCourseFromTag,
     getPostType,
     extractPostType,
     findPostType,
     getDepartmentName,
     isDepartmentName,
     parsingAlgoOne) where

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

-- Post Parsing

getPostType :: T.Text -> String
getPostType postCode =
    let codeSection = extractPostType (T.unpack postCode)
    in
        case codeSection of
            "SPE" -> "Specialist"
            "MAJ" -> "Major"
            "MIN" ->  "Minor"

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

getDepartmentName :: String -> T.Text -> String
getDepartmentName fullPostName postType = do
    let parsed = P.parse (isDepartmentName (T.unpack postType)) "(source)" fullPostName
    case parsed of
        Right name -> name
        Left _ -> ""

isDepartmentName ::  [Char] -> Parser String
isDepartmentName postType = parseUntil (P.string postType)

-- Post Category Parsing

parsingAlgoOne :: Maybe String -> Parser [String]
parsingAlgoOne firstCourse = do
    getRequirements firstCourse
    splitPrereqText

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
