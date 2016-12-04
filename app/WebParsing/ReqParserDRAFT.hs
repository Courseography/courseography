{-# LANGUAGE FlexibleContexts #-}
module WebParsing.ReqParser
    (andorParser) where

import qualified Text.Parsec as Parsec
import Text.Parsec.String
import Text.Parsec ((<|>))
import Control.Monad.Identity (Identity)
import qualified Data.String as S
import qualified Data.List as L
parse rule text = Parsec.parse rule "(source)" text


data ProgramReq = PRGREQ String [Req]

-- Returns a well formatted String representing a program requirement for specified program.
instance Show ProgramReq where
    show (PRGREQ program reqs) = "Program Requirements for " ++ program
                               ++ ":\n" ++ L.intercalate "\n" (map show reqs)

data CourseReq = CRSREQ String Req Req Req

instance Show CourseReq where
    show (CRSREQ course creq excl preq) = "Corequisites for " ++ course ++ ":\n"
                                        ++ show creq ++ "\n" ++ "Exclusions for "
                                        ++ course ++ ":\n" ++ show excl ++ "\n"
                                        ++ "Prerequisites for " ++ course ++ ":\n"
                                        ++ show preq ++ "\n"

-- define separators for "from"
-- for now J seems to be most readable and convenient value constructor for satisfying rec structure.
data Req = J String | AND [Req] | OR [Req] | FROM String Req | GRADE String Req

instance Show Req where
    show (J course) = course
    show (AND reqs) =
        case reqs of
        [x] -> show x
        otherwise -> "(" ++ (L.intercalate "," $ map show reqs) ++ ")"
    show (OR reqs) =
        case reqs of
        [x] -> show x
        otherwise -> "(" ++ (L.intercalate "/" $ map show reqs) ++ ")"
    show (FROM fces reqs) =  fces ++ " FCE(s) from:\n" ++ show reqs
    show (GRADE grade reqs) =  "(" ++ show reqs ++ " with a minimum grade of " ++ grade ++ "%)"

-- define separators
fromSeparator :: Parsec.Parsec String () ()
fromSeparator = Parsec.spaces >> (Parsec.try (Parsec.string "FCE") 
             <|> (Parsec.string "FCEs")) >> Parsec.spaces

lpSeparator :: Parsec.Parsec String () ()
lpSeparator = Parsec.spaces >> Parsec.char '(' >> Parsec.spaces

rpSeparator :: Parsec.Parsec String () ()
rpSeparator = Parsec.spaces >> Parsec.char ')' >> Parsec.spaces

orSeparator :: Parsec.Parsec String () ()
orSeparator = Parsec.spaces >> (Parsec.try (Parsec.string "/") <|> (Parsec.string "OR")
                                 <|> (Parsec.string "Or") <|> (Parsec.string "or")) >> Parsec.spaces

andSeparator :: Parsec.Parsec String () ()
andSeparator = Parsec.spaces >> (Parsec.try (Parsec.string ",") <|> (Parsec.string "AND")
                                 <|> (Parsec.string "And") <|> (Parsec.string "and")) >> Parsec.spaces

semcolSeparator :: Parsec.Parsec String () ()
semcolSeparator = Parsec.spaces >> Parsec.char ';' >> Parsec.spaces

floatParser :: Parsec.Parsec String () String
floatParser = do
    int <- Parsec.digit
    Parsec.char '.'
    float <- Parsec.digit
    return [int,'.',float]

intParser :: Parsec.Parsec String () String
intParser = do
    fces <- Parsec.digit
    return [fces]

fcesParser :: Parsec.Parsec String () String
fcesParser = do
    Parsec.manyTill (Parsec.anyChar) (Parsec.try (Parsec.lookAhead (Parsec.try floatParser <|> intParser)))
    fces <- Parsec.try floatParser <|> intParser
    return fces

percentParser :: Parsec.Parsec String () String
percentParser = Parsec.try percentParser3 <|> percentParser2 <|> percentParser1

percentParser1 :: Parsec.Parsec String () String
percentParser1 = do
    Parsec.spaces
    fces <- Parsec.count 2 Parsec.digit
    Parsec.many (Parsec.char '%')
    Parsec.spaces
    return fces

percentParser2 :: Parsec.Parsec String () String
percentParser2 = do
    Parsec.spaces
    Parsec.char '('
    fces <- Parsec.count 2 Parsec.digit
    Parsec.many (Parsec.char '%')
    Parsec.char ')'
    Parsec.spaces
    return fces

percentParser3 :: Parsec.Parsec String () String
percentParser3 = do
    Parsec.spaces
    Parsec.char '('
    Parsec.spaces
    Parsec.manyTill (Parsec.try (Parsec.noneOf ",/():;")) (Parsec.try (Parsec.lookAhead (Parsec.digit)))
    fces <- Parsec.count 2 Parsec.digit
    Parsec.many (Parsec.char '%')
    Parsec.char ')'
    Parsec.spaces
    return fces

letterParser :: Parsec.Parsec String () String
letterParser = do
    Parsec.manyTill (Parsec.try (Parsec.noneOf ",/():;")) (Parsec.try (Parsec.lookAhead
                     (Parsec.oneOf "ABCDEFabcdef"
                     >> Parsec.oneOf "+-")))
    letter <- Parsec.oneOf "ABCDEFabcdef"
    plusminus <- Parsec.oneOf "+-"
    Parsec.spaces
    return [letter,plusminus]

-- parse for cutoff percentage before a course
coBefParser :: Parsec.Parsec String () Req
coBefParser = do
    Parsec.spaces
    Parsec.manyTill (Parsec.try (Parsec.noneOf ",/():;")) (Parsec.try (Parsec.lookAhead (Parsec.try
                     letterParser <|> percentParser)))
    grade <- Parsec.try percentParser <|> letterParser
    Parsec.spaces
    Parsec.manyTill (Parsec.try (Parsec.noneOf ",/():;")) (Parsec.try (Parsec.lookAhead (singleParser)))
    req <- singleParser
    return $ GRADE grade req

-- parse for cutoff percentage after a course
coAftParser :: Parsec.Parsec String () Req
coAftParser = do
    Parsec.spaces
    Parsec.manyTill (Parsec.try (Parsec.noneOf ",/():;")) (Parsec.try (Parsec.lookAhead (singleParser)))
    req <- singleParser
    Parsec.spaces
    Parsec.manyTill (Parsec.try (Parsec.noneOf ",/():;")) (Parsec.try (Parsec.lookAhead (Parsec.try
                     letterParser <|> percentParser)))
    grade <- Parsec.try percentParser <|> letterParser
    Parsec.spaces
    return $ GRADE grade req

-- cutoff parser
cutoffParser :: Parsec.Parsec String () Req
cutoffParser = Parsec.try (coAftParser) <|> (coBefParser)

-- parse for reqs within parantheses
parParser :: Parsec.Parsec String () Req
parParser = do
    lpSeparator
    req <- Parsec.try cutoffParser <|> andorParser
    rpSeparator
    return req

-- parse for single course with "junk" data
junkParser :: Parsec.Parsec String () Req
junkParser = do
    junk1 <- Parsec.manyTill (Parsec.anyChar) (Parsec.try (Parsec.lookAhead (crsIDParser)))
    reqs <- crsIDParser
    junk2 <- Parsec.manyTill Parsec.anyChar (Parsec.try (Parsec.notFollowedBy (Parsec.try
                                             (Parsec.noneOf ",/():;\r\n"))))
    return $ J (junk1++reqs++junk2)

-- parse for single course ID
crsIDParser :: Parsec.Parsec String () String
crsIDParser = do
    Parsec.spaces
    -- with no spaces, we expect 3 letters, 3 digits, and (h/H/y/Y)1
    code <- Parsec.count 3 Parsec.letter
    num <- Parsec.count 3 Parsec.digit
    sess <- Parsec.count 2 Parsec.alphaNum
    Parsec.spaces
    return (code++num++sess)

-- parse for single course
singleParser :: Parsec.Parsec String () Req
singleParser = do
    Parsec.spaces
    -- with no spaces, we expect 3 letters, 3 digits, and (h/H/y/Y)1
    code <- Parsec.count 3 Parsec.letter
    num <- Parsec.count 3 Parsec.digit
    sess <- Parsec.count 2 Parsec.alphaNum
    Parsec.spaces
    return $ J (code++num++sess)

-- parse for single course with our without cutoff OR a req within parantheses
courseParser :: Parsec.Parsec String () Req
courseParser = Parsec.try (parParser) <|> (Parsec.try cutoffParser <|> junkParser <|> singleParser)

-- parse for reqs separated by / "or"
orParser :: Parsec.Parsec String () Req
orParser = do
    reqs <- Parsec.sepBy (courseParser) (orSeparator)
    return $ OR reqs

-- parse for reqs separated by , "and"
andorParser :: Parsec.Parsec String () Req
andorParser = Parsec.try (fromParser) <|> (do
    reqs <- Parsec.sepBy (orParser) (andSeparator)
    return $ AND reqs)

-- parse for reqs in "from" format
fromParser :: Parsec.Parsec String () Req
fromParser = do 
    fces <- fcesParser
    Parsec.manyTill (Parsec.letter) fromSeparator
    Parsec.manyTill (Parsec.anyChar) (Parsec.try (Parsec.lookAhead (courseParser)))
    req <- andorParser
    return $ FROM fces req

-- parse for reqs separated by ; "and?"
-- semicolons separate reqs and should have the highest precedence.. simply split by ; with andorParser
semcolParser :: Parsec.Parsec String () Req
semcolParser = do
    reqs <- Parsec.sepBy (andorParser) (Parsec.char ';')
    return $ AND reqs

-- highest level req parser.
reqParser :: String -> Req
reqParser string =
    let req = parse semcolParser string
    in case req of
        Right x -> x
        Left _ -> J "ERROR"

    -- [x] fix cutoffParser
    -- [X] cutoffParser can now deal with cutoffs BEFORE a req
    -- [X] integrate cutoffParser into recursive structure
    -- [X] get andorParser to parse consecutive cutoffs
    -- [X] works with english, added more functionality to separators
    -- [X] fix fromParser to handle text between from and course req
    -- [X] Fix fromSeparator; FROM ISNT EVERYWHERE, BUT FCES IS.
    -- [X] Make cutoffParser work with english...
    -- [X] PercentParser works for % and not %
    -- [X]JUNKPARSER
    -- [X] cutoff parser for letter grades
    -- CSC165H1/CSC236H1/CSC240H1 (with a minimum grade of 60%), 
    -- [X] CSC436H1/(CSC336H1 (75%))", nested cutoff
    -- [] create Group Type
    -- [] year (Done by Christine)
