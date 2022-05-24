{-# LANGUAGE FlexibleContexts #-}
module WebParsing.ReqParser where

import Data.Char (isSpace, toLower, toUpper)
import Database.Requirement
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)

-- define separators
fromSeparator :: Parser String
fromSeparator = Parsec.spaces
                >> Parsec.choice (map (Parsec.try . Parsec.string) [
            "of any of the following:",
            "from the following: ",
            "from:",
            "from",
            "at"
    ])

completionPrefix :: Parser ()
completionPrefix = Parsec.choice (map (Parsec.try . Parsec.string) [
    "Completion of at least",
    "Completion of a minimum of",
    "Completion of"
    ])
    >> Parsec.spaces

programPrefix :: Parser ()
programPrefix = Parsec.choice (map caseInsensitiveStr [
    "admission to",
    "enrolment in the ",
    "enrolment in an ",
    "enrolment in a ", -- trailing space to prevent matching "enrolment in A..."
    "enrolment in"
    ])
    >> Parsec.spaces

degreeType :: Parser String
degreeType = do
    Parsec.spaces
    degree <- Parsec.choice $ map caseInsensitiveStr [
        "major",
        "specialist"
        ]
    Parsec.spaces
    return degree

programSuffix :: Parser String
programSuffix = Parsec.choice $ map caseInsensitiveStr [
    " program of study",
    " program"
    ]

fceSeparator :: Parser ()
fceSeparator = Parsec.choice (map (Parsec.try . Parsec.string) [
            "FCEs.",
            "FCEs",
            "FCE.",
            "FCE",
            "credits",
            "full-course equivalents"
            ])
            >> Parsec.spaces

includingSeparator :: Parser String
includingSeparator = Parsec.optional (Parsec.string ",")
                     >> Parsec.spaces
                     >> Parsec.string "including"


lParen :: Parser Char
lParen = Parsec.char '('

rParen :: Parser Char
rParen = Parsec.char ')'

orSeparator :: Parser String
orSeparator = Parsec.choice $ map caseInsensitiveStr [
    "/",
    "or",
    ", or"
    ]

andSeparator :: Parser String
andSeparator = Parsec.choice $ map caseInsensitiveStr [
    ", and",
    ",",
    "and",
    "; and",
    ";",
    "&",
    "+",
    "plus"
    ]

progGroupSeparator :: Parser String
progGroupSeparator = Parsec.choice $ map (Parsec.try . Parsec.string) [
    ",",
    " or a ", -- frontal space to prevent matching "history"
    " or "    -- trailing space to prevent matching "organization"
    ]

progOrSeparator :: Parser String
progOrSeparator = Parsec.choice $ map caseInsensitiveStr [
    " or in a ", -- trailing space to prevent matching program name starting with "a"
    ", or "
    ]

oneOfSeparator :: Parser String
oneOfSeparator = do
    separator <- Parsec.choice $ map caseInsensitiveStr [
        "one of either",
        "one of the following",
        "at least one of",
        "one of",
        "1 of",
        "at least 1 of"
        ]
    colon <- Parsec.option "" $ Parsec.string ":"
    return (separator ++ colon)


semicolon :: Parser Char
semicolon = Parsec.char ';'

caseInsensitiveChar :: Parsec.Stream s m Char => Char -> Parsec.ParsecT s u m Char
caseInsensitiveChar c = Parsec.char (toLower c) <|> Parsec.char (toUpper c)

-- Match the string 's' regardless of the case of each character
caseInsensitiveStr :: Parsec.Stream s m Char => String -> Parsec.ParsecT s u m String
caseInsensitiveStr s = Parsec.try (mapM caseInsensitiveChar s)

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
    fces <- Parsec.try (do
        percent <- Parsec.count 2 Parsec.digit
        Parsec.notFollowedBy Parsec.digit
        return percent)
    Parsec.optional (Parsec.char '%')
    return fces

letterParser :: Parser String
letterParser = do
    letter <- Parsec.try (do
        letterGrade <- Parsec.oneOf "ABCDEF"
        Parsec.notFollowedBy Parsec.letter
        return letterGrade)
    plusminus <- Parsec.option "" $ Parsec.string "+" <|> Parsec.string "-"
    return $ letter : plusminus

infoParser :: Parser String
infoParser= do
    Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.lookAhead $ Parsec.string ")")


-- | Parser for a grade, which can be in one of the following forms:
-- a number with or without a percent symbol, or a letter A-F followed by a +/-.
gradeParser :: Parser String
gradeParser = do
    grade <- (Parsec.between lParen rParen percentParser <|> letterParser) <|> percentParser <|> letterParser
    _ <- Parsec.try $ Parsec.lookAhead $ Parsec.choice $ map Parsec.try [
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
    _ <- Parsec.optional (caseInsensitiveStr "an " <|> Parsec.try indefiniteArticleAParser)
    Parsec.spaces
    grade <- cutoffHelper <|> gradeParser
    Parsec.spaces
    _ <- Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.lookAhead singleParser)
    GRADE grade <$> singleParser

    where
    cutoffHelper = do
        _ <- Parsec.choice $ map (Parsec.try . caseInsensitiveStr)
                ["minimum grade", "minimum mark", "minimum", "grade", "final grade", "at least"]
        Parsec.spaces
        _ <- Parsec.optional $ caseInsensitiveStr "of"
        Parsec.spaces
        gradeParser

    indefiniteArticleAParser = do
        indefiniteArticle <- caseInsensitiveStr "a "
        Parsec.notFollowedBy $ caseInsensitiveStr "in"
        return indefiniteArticle

-- parse for cutoff percentage after a course
coAftParser :: Parser Req
coAftParser = do
    req <- singleParser
    Parsec.spaces
    grade <- Parsec.between lParen rParen parenCutoffHelper <|> cutoffHelper
    return $ GRADE grade req

    where
    parenCutoffHelper = do
        grade <- cutoffHelper
        _ <- Parsec.optional $ Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.lookAhead rParen)
        return grade

    -- Parse for the cutoff percentage, which may be followed by "or higher" or "or more". These "or"s will be skipped.
    cutoffHelper = do
        _ <- Parsec.manyTill (Parsec.noneOf "()")
            (Parsec.try $ Parsec.lookAhead (orSeparator <|> andSeparator <|> (do
                _ <- gradeParser
                Parsec.notFollowedBy Parsec.alphaNum
                return "")))
        grade <- gradeParser
        Parsec.spaces
        _ <- Parsec.optional $ caseInsensitiveStr "or higher" <|> caseInsensitiveStr "or more"
        return grade

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
        Parsec.try (fmap Left percentParser <|> fmap Left letterParser <|> fmap Right infoParser)

-- Parses for single course or a group of programs with or without cutoff OR a req within parentheses
-- Programs need to be parsed in groups because of the concatenation issue
-- explained in the docstring of `programGroupParser`
courseOrProgParser :: Parser Req
courseOrProgParser = Parsec.between Parsec.spaces Parsec.spaces $ Parsec.choice $ map Parsec.try [
    parParser,
    cutoffParser,
    justParser,
    programOrParser,
    rawTextParser
    ]

programParser :: Parser Req
programParser = do
    Parsec.spaces
    program <- Parsec.manyTill Parsec.anyChar $ Parsec.choice $ map (Parsec.try . Parsec.lookAhead) [
        degreeType,
        programSuffix,
        progGroupSeparator,
        progOrSeparator,
        Parsec.oneOf ".;" >> return "",
        Parsec.eof >> return ""
        ]
    return $ PROGRAM program

-- Flattens nested ORs into a single OR
-- eg. OR [OR ["CS major, "Math major"], RAW "permission from instructor"]
-- Nested ORs occur because the way programs are related through ORs is
-- different than that of courses. So they each have their orParser, which
-- may be related throuhg another OR
flattenOr :: [Req] -> [Req]
flattenOr [] = []
flattenOr (OR x:xs) = x ++ flattenOr xs
flattenOr (x:xs) = x:flattenOr xs

-- | Parser for reqs related through the "one of the following" condition
-- | Courses that fall under the one of condition may be separated by orSeparators or andSeparators
oneOfParser :: Parser Req
oneOfParser = do
    Parsec.spaces
    _ <- Parsec.try oneOfSeparator
    Parsec.spaces
    reqs <- Parsec.sepBy courseOrProgParser (Parsec.try orSeparator <|> Parsec.try andSeparator)
    case reqs of
        [] -> fail "Empty Req."
        [x] -> return x
        (x:xs) -> return $ OR $ flattenOr (x:xs)

-- | Parser for reqs related through an OR.
orParser :: Parser Req
orParser = do
    reqs <- Parsec.sepBy courseOrProgParser orSeparator
    case reqs of
        [] -> fail "Empty Req."
        [x] -> return x
        (x:xs) -> return $ OR $ flattenOr (x:xs)

-- | Parser for for reqs related through an AND.
andParser :: Parser Req
andParser = do
    reqs <- Parsec.sepBy (Parsec.try oneOfParser <|> orParser <|> programOrParser) andSeparator
    case reqs of
        [] -> fail "Empty Req."
        [x] -> return x
        (x:xs) -> return $ AND (x:xs)

-- | Parser for programs grouped together
-- | Parses program names and degree types, then concatenate every combination
-- | eg. (CS or MAT major) implies (CS major) or (MAT major)
-- | (CS major or specialist) implies (CS major) or (CS specialist)
-- | (CS or MAT major or specialist) implies (CS maj) or (MAT maj) or (CS spec) or (MAT spec)
programGroupParser :: Parser Req
programGroupParser = do
    progs <- Parsec.sepBy (Parsec.try programParser) progGroupSeparator
    degrees <- Parsec.sepBy (Parsec.try degreeType) (Parsec.string "or")
    _ <- Parsec.optional $ Parsec.choice $ map Parsec.try [
        degreeType,
        programSuffix
        ]

    case progs of
        [] -> fail "Empty Req."
        [PROGRAM x] -> case degrees of
            [] -> return $ PROGRAM x
            [d] -> return $ PROGRAM (x ++ " " ++ d)
            ds -> return $ OR [PROGRAM (x ++ " " ++ d) | d <- ds]
        xs -> case degrees of
            [] -> return $ OR [PROGRAM x | PROGRAM x <- xs]
            ds -> return $ OR [PROGRAM (x ++ " " ++ d) | PROGRAM x <- xs, d <- ds]

programOrParser :: Parser Req
programOrParser = do
    Parsec.spaces
    _ <- programPrefix
    progs <- Parsec.sepBy (Parsec.try programGroupParser) progOrSeparator
    _ <- Parsec.many $ Parsec.noneOf ".;,)"
    case progs of
        [] -> fail "Empty Req."
        [x] -> return x
        (x:xs) -> return $ OR $ flattenOr (x:xs)

-- | Parser for FCE requirements:
-- "... 9.0 FCEs ..."
fcesParser :: Parser Req
fcesParser = do
    _ <- Parsec.optional completionPrefix
    fces <- creditsParser
    _ <- Parsec.spaces
    _ <- fceSeparator
    _ <- Parsec.optional $ Parsec.try includingSeparator <|> Parsec.try fromSeparator
    req <- Parsec.try andParser <|> Parsec.try orParser
    return $ FCES fces req

-- | Parser for requirements separated by a semicolon.
categoryParser :: Parser Req
categoryParser = Parsec.try fcesParser <|> Parsec.try andParser

parseReqs :: String -> Req
parseReqs reqString = do
    let reqStringLower = map toLower reqString
    if all isSpace reqString || reqStringLower == "none" || reqStringLower == "no"
        then NONE
        else do
            let req = Parsec.parse categoryParser "" reqString
                in case req of
                    Right x -> x
                    Left e -> J (show e) ""
