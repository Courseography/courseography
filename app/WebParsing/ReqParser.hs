{-# LANGUAGE FlexibleContexts #-}
module WebParsing.ReqParser where

import Control.Monad
import Data.Char (isSpace, toLower, toUpper)
import Database.Requirement
import Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)

-- | a space or a zero-width space (unicode: 8203)
space :: Parser Char
space = Parsec.space <|> Parsec.char '\8203'

spaces :: Parser ()
spaces = Control.Monad.void (Parsec.many space)

-- define separators
fromSeparator :: Parser String
fromSeparator = spaces
                >> Parsec.choice (map caseInsensitiveStr [
            "of any of the following:",
            "of",
            "from the following: ",
            "from the",
            "from a",
            "from:",
            "from",
            "must be at the",
            "at the",
            "at",
            "in"
    ])

completionPrefix :: Parser ()
completionPrefix = Parsec.choice (map caseInsensitiveStr [
    "Completion of at least",
    "Completion of a minimum of",
    "Completion of",
    "have completed",
    "At least one additional",
    "At least one",
    "At least",
    "Any",
    "a"
    ])
    >> Parsec.skipMany1 space

cgpaPrefix :: Parser ()
cgpaPrefix = Parsec.choice (map caseInsensitiveStr [
    "and will normally have a CGPA of at least",
    "with a CGPA of at least",
    "with a minimum cGPA of",
    "and a minimum cGPA of",
    "and minimum cGPA of",
    "a CGPA of at least",
    "a minimum cGPA of",
    "minimum cGPA of",
    "with",
    "cGPA"
    ])
    >> Parsec.skipMany1 space

programPrefix :: Parser ()
programPrefix = Parsec.choice (map caseInsensitiveStr [
    "admission to",
    "enrolment in the",
    "enrolment in an",
    "enrolment in a",
    "enrolment in"
    ])
    >> Parsec.skipMany1 space

degreeType :: Parser String
degreeType = Parsec.between spaces spaces $ Parsec.choice $ map caseInsensitiveStr [
    "major",
    "minor",
    "specialist"
    ]

programSuffix :: Parser String
programSuffix = spaces
    >> Parsec.choice (map caseInsensitiveStr [
        "program of study",
        "program"
        ])

fceSeparator :: Parser ()
fceSeparator = Parsec.choice (map caseInsensitiveStr [
            "FCEs.",
            "FCEs",
            "FCE.",
            "FCE",
            "credits",
            "credit",
            "full-course equivalents",
            "additional credits",
            "additional credit"
            ])
            >> spaces

includingSeparator :: Parser String
includingSeparator = Parsec.optional (Parsec.string ",")
                     >> spaces
                     >> caseInsensitiveStr "including"


lParen :: Parser Char
lParen = Parsec.char '('

rParen :: Parser Char
rParen = Parsec.char ')'

orSeparator :: Parser String
orSeparator = Parsec.choice [
    caseInsensitiveStr "/",
    spaces >> caseInsensitiveStr "or",
    caseInsensitiveStr ", or"
    ]

andSeparator :: Parser String
andSeparator = Parsec.choice $ map caseInsensitiveStr [
    ", and",
    ", an additional",
    ", additional",
    ",",
    "and",
    "; and",
    ".",
    ";",
    "&",
    "+",
    "plus"
    ]

progGroupSeparator :: Parser String
progGroupSeparator = do
    separator <- Parsec.choice (map Parsec.try [
        Parsec.string ",",
        space >> caseInsensitiveStr "or a",
        space >> caseInsensitiveStr "or"
        ])
    _ <- space
    return $ separator ++ " "

progOrSeparator :: Parser String
progOrSeparator = do
    separator <- space >> caseInsensitiveStr "or in a"
    _ <- space
    return $ separator ++ " "

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


caseInsensitiveChar :: Parsec.Stream s m Char => Char -> Parsec.ParsecT s u m Char
caseInsensitiveChar c = Parsec.char (toLower c) <|> Parsec.char (toUpper c)

-- Match the string 's' regardless of the case of each character
caseInsensitiveStr :: Parsec.Stream s m Char => String -> Parsec.ParsecT s u m String
caseInsensitiveStr s = Parsec.try (mapM caseInsensitiveChar s)

creditsParser :: Parser Float
creditsParser = do
    spaces
    integral <- Parsec.many1 Parsec.digit
    point <- Parsec.option "" $ Parsec.string "."
    fractional <- if point == "" then return "" else Parsec.many1 Parsec.digit
    return $ read $ integral ++ point ++ fractional

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
infoParser = Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.lookAhead $ Parsec.string ")")


-- | Parser for a grade, which can be in one of the following forms:
-- a number with or without a percent symbol, or a letter A-F followed by a +/-.
gradeParser :: Parser String
gradeParser = do
    grade <- Parsec.between lParen rParen (percentParser <|> letterParser) <|> percentParser <|> letterParser
    _ <- Parsec.choice $ map (Parsec.try . Parsec.lookAhead) [
        andSeparator,
        orSeparator,
        space >> return "",
        Parsec.eof >> return "",
        lParen >> return "",
        rParen >> return ""
        ]
    return grade

-- parse for cutoff percentage before a course
coBefParser :: Parser Req
coBefParser = do
    _ <- Parsec.optional (caseInsensitiveStr "an " <|> Parsec.try indefiniteArticleAParser)
    spaces
    grade <- cutoffHelper <|> gradeParser
    spaces
    _ <- Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.lookAhead singleParser)
    Grade grade <$> singleParser

    where
    cutoffHelper = do
        _ <- Parsec.choice $ map caseInsensitiveStr
                ["minimum grade", "minimum mark", "minimum", "grade", "final grade", "at least"]
        spaces
        _ <- Parsec.optional $ caseInsensitiveStr "of"
        spaces
        gradeParser

    indefiniteArticleAParser = do
        indefiniteArticle <- caseInsensitiveStr "a "
        Parsec.notFollowedBy $ caseInsensitiveStr "in"
        return indefiniteArticle

-- parse for cutoff percentage after a course
coAftParser :: Parser Req
coAftParser = do
    req <- singleParser
    spaces
    grade <- Parsec.between lParen rParen parenCutoffHelper <|> cutoffHelper
    return $ Grade grade req

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
        spaces
        _ <- Parsec.optional $ caseInsensitiveStr "or higher" <|> caseInsensitiveStr "or more"
        return grade

-- | Parser for a grade cutoff on a course.
-- This is tricky because the cutoff can come before or after the course code.
cutoffParser :: Parser Req
cutoffParser = Parsec.try coAftParser <|> coBefParser

-- | Parser for requirements written within parentheses
parParser :: Parser Req
parParser = Parsec.between lParen rParen reqParser

-- | Parser for raw text in a prerequisite, e.g., "proficiency in C/C++".
-- Note that even if a course code appears in the middle of such text,
-- this code is treated as plain text.
rawTextParser :: Parser Req
rawTextParser = do
    text <- Parsec.many $ Parsec.noneOf ";\r\n"
    return $ Raw text

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
    spaces
    courseID <- courseIDParser
    spaces
    meta <- Parsec.option (Right "") $ Parsec.between lParen rParen markInfoParser
    return $ case meta of
        Left mark -> Grade mark $ J courseID ""
        Right info -> J courseID info
    where
    markInfoParser :: Parser (Either String String)
    markInfoParser = do
        Parsec.try (fmap Left percentParser <|> fmap Left letterParser <|> fmap Right infoParser)

-- parse for single course with or without cutoff ReqOr a req within parentheses
courseParser :: Parser Req
courseParser = Parsec.choice $ map Parsec.try [
    parParser,
    cutoffParser,
    justParser
    ]

-- Parses for a single course or a group of programs
-- Programs need to be parsed in groups because of the concatenation issue
-- explained in the docstring of `programGroupParser`
categoryParser :: Parser Req
categoryParser = Parsec.between spaces spaces $ Parsec.choice $ map Parsec.try [
    fcesParser,
    courseParser,
    cgpaParser,
    programOrParser,
    rawTextParser
    ]

programParser :: Parser Req
programParser = do
    spaces
    program <- Parsec.manyTill Parsec.anyChar $ Parsec.choice $ map (Parsec.try . Parsec.lookAhead) [
        degreeType,
        programSuffix,
        progGroupSeparator,
        progOrSeparator,
        Parsec.oneOf ".;" >> return "",
        Parsec.eof >> return ""
        ]
    return $ Program program

-- | Given a parser p, returns a parser that parses one or more p related through
-- | the "one of the following" condition
-- | Courses that fall under the one of condition may be separated by orSeparators or andSeparators
oneOfParser :: Parser Req -> Parser Req
oneOfParser p = do
    spaces
    _ <- Parsec.try oneOfSeparator
    spaces
    reqs <- Parsec.sepBy p (Parsec.try orSeparator <|> Parsec.try andSeparator)
    case reqs of
        [] -> fail "Empty Req."
        [x] -> return x
        (x:xs) -> return $ ReqOr $ flattenOr (x:xs)

-- | Returns a parser that parses ReqOrs of the given parser
orParser :: Parser Req -> Parser Req
orParser p = do
    reqs <- Parsec.sepBy p orSeparator
    case reqs of
        [] -> fail "Empty Req."
        [x] -> return x
        (x:xs) -> return $ ReqOr $ flattenOr (x:xs)

-- | Returns a parser that parses ReqAnds of ReqOrs of the given parser
andParser :: Parser Req -> Parser Req
andParser p = do
    reqs <- Parsec.sepBy (Parsec.try (oneOfParser p) <|> Parsec.try (orParser p)) andSeparator
    case reqs of
        [] -> fail "Empty Req."
        [x] -> return x
        (x:[Raw ""]) -> return x
        (x:xs) -> return $ ReqAnd $ flattenAnd (x:xs)

-- | Parser for programs grouped together
-- | Parses program names and degree types, then concatenate every combination
-- | eg. (CS or MAT major) implies (CS major) or (MAT major)
-- | (CS major or specialist) implies (CS major) or (CS specialist)
-- | (CS or MAT major or specialist) implies (CS maj) or (MAT maj) or (CS spec) or (MAT spec)
programGroupParser :: Parser Req
programGroupParser = do
    progs <- Parsec.sepBy (Parsec.try programParser) progGroupSeparator
    degrees <- sepByNoConsume (Parsec.try degreeType) orSeparator
    _ <- Parsec.optional $ Parsec.choice $ map Parsec.try [
        degreeType,
        programSuffix
        ]

    case progs of
        [] -> fail "Empty Req."
        [Program x] -> case degrees of
            [] -> return $ Program x
            [d] -> return $ Program (x ++ " " ++ d)
            ds -> return $ ReqOr [Program (x ++ " " ++ d) | d <- ds]
        xs -> case degrees of
            [] -> return $ ReqOr [Program x | Program x <- xs]
            ds -> return $ ReqOr [Program (x ++ " " ++ d) | Program x <- xs, d <- ds]

programOrParser :: Parser Req
programOrParser = do
    spaces
    _ <- programPrefix
    progs <- Parsec.sepBy (Parsec.try programGroupParser) progOrSeparator

    case progs of
        [] -> fail "Empty Req."
        [x] -> return x
        (x:xs) -> return $ ReqOr $ flattenOr (x:xs)

-- | Parser for FCE requirements:
-- "... 9.0 FCEs ..."
fcesParser :: Parser Req
fcesParser = do
    _ <- Parsec.optional completionPrefix
    fces <- creditsParser
    dept <- Parsec.optionMaybe $ Parsec.try departmentParser
    _ <- spaces
    _ <- fceSeparator
    req <- Parsec.optionMaybe $ Parsec.try includingSeparator >> andParser categoryParser
    _ <- Parsec.optional $ Parsec.try fromSeparator
    _ <- spaces
    _ <- Parsec.optional $ Parsec.try anyModifierParser
    modifiers <- modAndParser

    let fcesReq = case dept of
            Nothing -> Fces fces modifiers
            Just x -> case modifiers of
                Requirement (Raw "") -> Fces fces x
                ModAnd ms -> Fces fces $ ModAnd (x:ms)
                _ -> Fces fces $ ModAnd [modifiers, x]

    case req of
        Nothing -> return fcesReq
        Just x -> return $ ReqAnd $ flattenAnd [fcesReq, x]

-- | Parser for Fces modifiers except for rawModifierParser
fcesModifiersParserNoRaw :: Parser Modifier
fcesModifiersParserNoRaw = Parsec.try courseAsModParser
    <|> Parsec.try levelParser
    <|> Parsec.try departmentParser

-- | Parser for Fces modifiers
fcesModifiersParser :: Parser Modifier
fcesModifiersParser = fcesModifiersParserNoRaw <|> rawModifierParser

-- | Parses fces modifiers related through and clauses
-- | Not using andParser and sepByNoConsume because empty strings are handled differently
modAndParser :: Parser Modifier
modAndParser = do
    x <- Parsec.try fcesModifiersParser

    case x of
        Requirement (Raw "") -> return x
        _ -> do
            xs <- Parsec.many $ Parsec.try $
                (fromSeparator <|> Parsec.string "") >> fcesModifiersParserNoRaw
            case xs of
                [] -> return x
                _ -> return $ ModAnd (x:xs)

-- | An andParser for courses but wraps the returned Req in a Modifier
courseAsModParser :: Parser Modifier
courseAsModParser = do
    req <- andParser courseParser
    return $ Requirement req

-- | Parses a literal "course" or "courses"
courseLiteralParser :: Parser String
courseLiteralParser = Parsec.choice (map caseInsensitiveStr [
    "courses",
    "course"
    ])

plainLevelParser :: Parser String
plainLevelParser = do
    level <- Parsec.count 3 Parsec.digit
    _ <- Parsec.optional $ Parsec.char '-'
    return level

-- | Parses a level modifier in the fces requirement
-- | eg. the "300-level" in "1.0 credit at the 300-level"
levelParser :: Parser Modifier
levelParser = do
    _ <- spaces
    levels <- sepByNoConsume plainLevelParser orSeparator
    plus <- Parsec.optionMaybe $ Parsec.char '+'
    _ <- spaces
    _ <- caseInsensitiveStr "level"
    _ <- spaces
    _ <- Parsec.optional $ Parsec.try courseLiteralParser
    higher <- Parsec.optionMaybe $ caseInsensitiveStr "or higher"

    case levels of
        [] -> fail "no level"
        [level] -> case plus of
            Nothing -> case higher of
                Nothing -> return $ Level level
                Just _ -> return $ Level $ level ++ "+"
            Just _ -> return $ Level $ level ++ "+"
        xs -> return $ ModOr $ map Level xs

-- | Parses a department code in the fces requirement
-- | eg. the "CSC" in "1.0 credit in CSC" or "1.0 credit in CSC courses"
departmentParser :: Parser Modifier
departmentParser = do
    let end = [
            courseLiteralParser,
            fceSeparator >> return "",
            plainLevelParser,
            orSeparator,
            andSeparator,
            fromSeparator,
            Parsec.eof >> return ""
            ]

    spaces
    Parsec.notFollowedBy fceSeparator
    Parsec.notFollowedBy $ Parsec.choice end
    depts <- Parsec.sepBy1
        (many1Till Parsec.anyChar $ Parsec.choice $ map (Parsec.try . Parsec.lookAhead) end)
        (Parsec.try orSeparator)
    spaces >> Parsec.optional courseLiteralParser

    case depts of
        [dept] ->return $ Department $ trim dept
        xs -> return $ ModOr $ map (Department . trim) xs

-- | Parser for the raw text in fcesParser
-- | Like rawTextParser but terminates at ands and ors
rawModifierParser :: Parser Modifier
rawModifierParser = do
    spaces
    text <- Parsec.manyTill Parsec.anyChar $ Parsec.try $ spaces
        >> Parsec.choice (map (Parsec.try . Parsec.lookAhead) [
            andSeparator,
            orSeparator,
            Parsec.eof >> return ""
            ])
    return $ Requirement $ Raw text

-- | Parses "any field" or "any subject" in an fces modifier since they are redundant
anyModifierParser :: Parser ()
anyModifierParser = caseInsensitiveStr "any"
    >> Parsec.many1 space
    >> Parsec.choice (map caseInsensitiveStr [
        "field",
        "subject"
        ])
    >> spaces

-- Parser for cGPA requirements: "... 1.0 cGPA ..."
cgpaParser :: Parser Req
cgpaParser = do
    _ <- Parsec.optional cgpaPrefix
    gpa <- creditsParser
    spaces
    _ <- Parsec.optional (caseInsensitiveStr "cGPA")
    spaces
    addendum <- Parsec.manyTill Parsec.anyChar $ Parsec.try $ spaces
        >> Parsec.choice (map (Parsec.try . Parsec.lookAhead) [
        andSeparator,
        orSeparator,
        Parsec.eof >> return ""
        ])
    return $ Gpa gpa addendum

-- | Parser for requirements separated by a semicolon.
reqParser :: Parser Req
reqParser = Parsec.try $ andParser categoryParser

-- Similar to Parsec.sepBy but stops when sep passes but p fails,
-- and doesn't consume failed characters
-- Modified from https://hackage.haskell.org/package/parsec-3.1.15.1/docs/src/Text.Parsec.Combinator.html#sepBy
sepByNoConsume :: Parser String -> Parser String -> Parser [String]
sepByNoConsume p sep = (do
    x <- p
    xs <- Parsec.many $ Parsec.try (sep >> p)
    return (x:xs))
    <|> return []

-- Flattens nested ReqOrs into a single ReqOr
-- eg. ReqOr [ReqOr ["CS major, "Math major"], Raw "permission from instructor"]
-- Nested ReqOrs occur because the way programs are related through ReqOrs is
-- different than that of courses. So they each have their orParser, which
-- may be related throuhg another ReqOr
flattenOr :: [Req] -> [Req]
flattenOr [] = []
flattenOr (ReqOr x:xs) = x ++ flattenOr xs
flattenOr (x:xs) = x:flattenOr xs

-- Flattens nested ReqAnds into a single ReqAnd
-- Similar to flattenOr (see above) but for ReqAnd
flattenAnd :: [Req] -> [Req]
flattenAnd [] = []
flattenAnd (ReqAnd x:xs) = x ++ flattenAnd xs
flattenAnd (x:xs) = x:flattenAnd xs

-- | Trims leading and trailing spaces from a string
-- | Modified from https://stackoverflow.com/a/6270337/10254049
-- | based on @Carcigenicate's comment
trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f

-- | Like Parsec.manyTill but consumes at least one character
many1Till :: Show b => Parser a -> Parser b -> Parser [a]
many1Till p end = do
    Parsec.notFollowedBy end
    x <- p
    xs <- Parsec.manyTill p end
    return $ x:xs

parseReqs :: String -> Req
parseReqs reqString = do
    let reqStringLower = map toLower reqString
    if all isSpace reqString || reqStringLower == "none" || reqStringLower == "no"
        then None
        else do
            let req = Parsec.parse reqParser "" reqString
                in case req of
                    Right x -> x
                    Left e -> J (show e) ""
