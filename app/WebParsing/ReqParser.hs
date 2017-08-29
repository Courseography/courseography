{-# LANGUAGE FlexibleContexts #-}
module WebParsing.ReqParser where

import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))
import Database.Requirement

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

creditsParser :: Parser String
creditsParser = do
    Parsec.spaces
    integral <- Parsec.many1 Parsec.digit
    point <- Parsec.option "" $ Parsec.string "."
    fractional <- if point == "" then return "" else Parsec.many1 Parsec.digit
    return $ integral ++ point ++ fractional

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

    where
    percentParser = do
        fces <- Parsec.many1 Parsec.digit
        Parsec.optional (Parsec.char '%')
        return fces

    letterParser = do
        letter <- Parsec.oneOf "ABCDEF"
        plusminus <- Parsec.option "" $ Parsec.string "+" <|> Parsec.string "-"
        return $ letter : plusminus

-- parse for cutoff percentage before a course
coBefParser :: Parser Req
coBefParser = do
    _ <- Parsec.choice $ map (Parsec.try . (>> Parsec.space) . Parsec.string) ["minimum grade of", "minimum mark of", "minimum of", "minimum"]
    Parsec.spaces
    grade <- gradeParser
    Parsec.spaces
    _ <- Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.lookAhead singleParser)
    req <- singleParser
    return $ GRADE grade req

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

-- | Parser for a single course.
-- We expect 3 letters, 3 digits, and a letter and a number.
singleParser :: Parser Req
singleParser = do
    code <- Parsec.count 3 Parsec.letter
    num <- Parsec.count 3 Parsec.digit
    -- TODO: Make the last two letters more restricted.
    sess <- Parsec.count 2 Parsec.alphaNum
    return $ J (code ++ num ++ sess)

-- parse for single course with our without cutoff OR a req within parantheses
courseParser :: Parser Req
courseParser = Parsec.between Parsec.spaces Parsec.spaces $ Parsec.choice $ map Parsec.try [
    parParser,
    cutoffParser,
    singleParser,
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
-- Semicolons are assumed to have the highest precedence.
categoryParser :: Parser Req
categoryParser = Parsec.try fcesParser <|> Parsec.try andParser
-- categoryParser :: Parser Req
-- categoryParser = do
--     reqs <- Parsec.sepBy (Parsec.try fcesParser <|> Parsec.try andParser) semicolon
--     Parsec.eof
--     case reqs of
--         [] -> fail "Empty Req."
--         [x] -> return x
--         (x:xs) -> return $ AND (x:xs)

-- | Parse the course requirements from a string.
parseReqs :: String -> Req
parseReqs reqString =
    let req = Parsec.parse categoryParser "" reqString
    in case req of
        Right x -> x
        Left e -> J (show e)
