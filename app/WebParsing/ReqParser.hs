{-# LANGUAGE FlexibleContexts #-}
module WebParsing.ReqParser where

import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>), (<?>))
import Database.Requirement
import Data.Char (toLower, toUpper, isSpace)

-- define separators
fromSeparator :: Parser ()
fromSeparator = Parsec.spaces >> (Parsec.choice $ map (Parsec.try . Parsec.string) [
            "full course or its equivalent",
            "FCEs",
            "FCE",
            "FCEs:",
            "FCE:"
    ]) >> (Parsec.choice $ map (Parsec.try . Parsec.string) [
            " of any of the following:",
            " from the following: ",
            " from:",
            " from",
            " at"
    ]) >> Parsec.spaces

lParen :: Parser Char
lParen = Parsec.char '('

rParen :: Parser Char
rParen = Parsec.char ')'

orSeparator :: Parser String
orSeparator = Parsec.choice $ map Parsec.string [
    "/",
    "OR",
    "Or",
    "or"
    ]

andSeparator :: Parser String
andSeparator = Parsec.choice $ map Parsec.string [
    ",",
    "AND",
    "And",
    "and",
    ";"
    ]

semicolon :: Parser Char
semicolon = Parsec.char ';'

caseInsensitiveChar :: Parsec.Stream s m Char => Char -> Parsec.ParsecT s u m Char
caseInsensitiveChar c = Parsec.char (toLower c) <|> Parsec.char (toUpper c)

-- Match the string 's' regardless of the case of each character
caseInsensitiveStr :: Parsec.Stream s m Char => String -> Parsec.ParsecT s u m String
caseInsensitiveStr s = Parsec.try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

creditsParser :: Parser String
creditsParser = do
    Parsec.spaces
    integral <- Parsec.many1 Parsec.digit
    point <- Parsec.option "" $ Parsec.string "."
    fractional <- if point == "" then return "" else Parsec.many1 Parsec.digit
    return $ integral ++ point ++ fractional

-- | Helpers for parsing grades
percentParser :: Parser String
percentParser = do
    fces <- Parsec.many1 Parsec.digit
    Parsec.optional (Parsec.char '%')
    return fces

letterParser :: Parser String
letterParser = do
    letter <- Parsec.oneOf "ABCDEF"
    plusminus <- Parsec.option "" $ Parsec.string "+" <|> Parsec.string "-"
    return $ letter : plusminus

infoParser :: Parser String
infoParser= do
    info <- Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.lookAhead $ Parsec.string ")")
    return $ info


-- | Parser for a grade, which can be in one of the following forms:
-- a number with or without a percent symbol, or a letter A-F followed by a +/-.
gradeParser :: Parser String
gradeParser = do
    grade <- Parsec.try ((Parsec.between lParen rParen percentParser <|> letterParser) <|> (percentParser <|> letterParser))
    _ <- Parsec.lookAhead $ Parsec.choice $ map Parsec.try [
        andSeparator,
        orSeparator,
        Parsec.space >> return "",
        Parsec.eof >> return "",
        Parsec.oneOf "(),/;" >> return ""
        ]
    return grade


-- parse for cutoff percentage before a course
coBefParser :: Parser Req
coBefParser = do
    _ <- Parsec.optional $ caseInsensitiveStr "an " <|> caseInsensitiveStr "a "
    grade <- cutoffHelper <|> gradeParser
    Parsec.spaces
    _ <- Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.lookAhead singleParser)
    req <- singleParser
    return $ GRADE grade req

    where
    cutoffHelper = do
        _ <- Parsec.choice $ map (Parsec.try . (>> Parsec.space) . caseInsensitiveStr)
                ["minimum grade", "minimum mark", "minimum", "grade", "final grade", "at least"]
        Parsec.spaces
        _ <- Parsec.optional $ caseInsensitiveStr "of"
        Parsec.spaces
        gradeParser

-- parse for cutoff percentage after a course
coAftParser :: Parser Req
coAftParser = do
    req <- singleParser
    Parsec.spaces
    grade <- Parsec.between lParen rParen cutoffHelper <|> cutoffHelper
    return $ GRADE grade req

    where
    cutoffHelper = Parsec.between Parsec.spaces Parsec.spaces $ do
        _ <- Parsec.manyTill (Parsec.noneOf "()")
          (Parsec.try $ Parsec.lookAhead (orSeparator <|> andSeparator <|> (do
            _ <- gradeParser
            Parsec.spaces
            Parsec.notFollowedBy $ Parsec.alphaNum
            return "")))
        gradeParser

-- | Parser for a grade cutoff on a course.
-- This is tricky because the cutoff can come before or after the course code.
cutoffParser :: Parser Req
cutoffParser = Parsec.try coAftParser <|> coBefParser

-- | Parser for requirements written within parentheses
parParser :: Parser Req
parParser = Parsec.between lParen rParen andParser

-- | Parser for raw text in a prerequisite, e.g., "proficiency in C/C++".
-- Note that even if a course code appears in the middle of such text,
-- this code is treated as plain text.
rawTextParser :: Parser Req
rawTextParser = do
    text <- Parsec.many $ Parsec.noneOf ";\r\n"
    return $ RAW text

-- | Parser for a single UTSG course.
-- We expect 3 letters, 3 digits
utsgCourseCodeParser :: Parser String
utsgCourseCodeParser = do
    code <- Parsec.count 3 Parsec.letter
    num <- Parsec.count 3 Parsec.digit
    return (code ++ num)

-- | Parser for a single UTSC course.
-- We expect 4 letters, 2 digits
utscCourseCodeParser :: Parser String
utscCourseCodeParser = do
    code <- Parsec.count 4 Parsec.letter
    num <- Parsec.count 2 Parsec.digit
    return (code ++ num)

-- | Parser for a single course.
-- We expect 3 letters followed by 3 digits or 4 letters followed by 2 digits, and a letter and a number.
courseIDParser :: Parser String
courseIDParser = do
    courseCode <- Parsec.try utsgCourseCodeParser <|> utscCourseCodeParser
    sess <- Parsec.string "H" <|> Parsec.string "Y"
    sessNum <- Parsec.digit
    return (courseCode ++ sess ++ [sessNum])

singleParser :: Parser Req
singleParser = do
    courseID <- courseIDParser
    return $ J courseID ""

-- | Parser for single courses or "atomic" Reqs represented by a J.
justParser :: Parser Req
justParser = do
    Parsec.spaces
    courseID <- courseIDParser
    Parsec.spaces
    meta <- Parsec.option (Right "") $ Parsec.between lParen rParen markInfoParser
    return $ case meta of
        Left mark -> GRADE mark $ J courseID ""
        Right info -> J courseID info
    where
    markInfoParser :: Parser (Either String String)
    markInfoParser = do
        grade <- Parsec.try (fmap Left percentParser <|> fmap Left letterParser <|> fmap Right infoParser)
        return grade

-- parse for single course with or without cutoff OR a req within parentheses
courseParser :: Parser Req
courseParser = Parsec.between Parsec.spaces Parsec.spaces $ Parsec.choice $ map Parsec.try [
    parParser,
    cutoffParser,
    justParser,
    rawTextParser
    ]

-- | Parser for reqs related through an OR.
orParser :: Parser Req
orParser = do
    reqs <- Parsec.sepBy courseParser orSeparator
    case reqs of
        [] -> fail "Empty Req."
        [x] -> return x
        (x:xs) -> return $ OR (x:xs)

-- | Parser for for reqs related through an AND.
andParser :: Parser Req
andParser = do
    reqs <- Parsec.sepBy orParser andSeparator
    case reqs of
        [] -> fail "Empty Req."
        [x] -> return x
        (x:xs) -> return $ AND (x:xs)

-- | Parser for reqs in "from" format:
-- 4.0 FCEs from CSC108H1, CSC148H1, ...
fcesParser :: Parser Req
fcesParser = do
    fces <- creditsParser
    _ <- fromSeparator
    Parsec.spaces
    req <- andParser
    return $ FCES fces req

-- | Parser for requirements separated by a semicolon.
categoryParser :: Parser Req
categoryParser = Parsec.try fcesParser <|> Parsec.try andParser

parseReqs :: String -> Req
parseReqs reqString = do
    let reqStringLower = [toLower c | c <- reqString]
    if all isSpace reqString || reqStringLower == "none" || reqStringLower == "no"
        then NONE
        else do
            let req = Parsec.parse categoryParser "" reqString
                in case req of
                    Right x -> x
                    Left e -> J (show e) ""
