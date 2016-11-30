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

-- define separators for "from"
fromSeparator :: Parsec.Parsec String () ()
fromSeparator = Parsec.spaces >> (Parsec.try (Parsec.string "FCE") 
             <|> (Parsec.string "FCEs")) >> Parsec.spaces

lpSeparator :: Parsec.Parsec String () ()
lpSeparator = Parsec.spaces >> Parsec.char '(' >> Parsec.spaces

rpSeparator :: Parsec.Parsec String () ()
rpSeparator = Parsec.spaces >> Parsec.char ')' >> Parsec.spaces

orSeparator :: Parsec.Parsec String () ()
orSeparator = Parsec.spaces >> (Parsec.try (Parsec.string "/") <|> (Parsec.string "OR") <|> (Parsec.string "Or") <|> (Parsec.string "or")) >> Parsec.spaces

andSeparator :: Parsec.Parsec String () ()
andSeparator = Parsec.spaces >> (Parsec.try (Parsec.string ",") <|> (Parsec.string "AND") <|> (Parsec.string "And") <|> (Parsec.string "and")) >> Parsec.spaces

semcolSeparator :: Parsec.Parsec String () ()
semcolSeparator = Parsec.spaces >> Parsec.char ';' >> Parsec.spaces

-- helper to find length of list of reqs
length_list :: [Req] -> Int
length_list [] = 0
length_list (x:xs) =1 + length_list xs

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
    Parsec.many (Parsec.noneOf "0123456789,/();\r\nandAndANDorOrORfromFromFROM")
    fces <- Parsec.try floatParser <|> intParser
    return fces

percentParser :: Parsec.Parsec String () String
percentParser = Parsec.try percentParser1 <|> percentParser2

percentParser1 :: Parsec.Parsec String () String
percentParser1 = do
    fces <- Parsec.count 2 Parsec.digit
    Parsec.char '%'
    return fces

percentParser2 :: Parsec.Parsec String () String
percentParser2 = do
    Parsec.char '('
    fces <- Parsec.count 2 Parsec.digit
    Parsec.char '%'
    Parsec.char ')'
    return fces

letterParser :: Parsec.Parsec String () String
letterParser = do
    letter <- Parsec.oneOf "ABCDEFabcdef"
    plusminus <- Parsec.oneOf "+-"
    return [letter,plusminus]

-- parse for cutoff percentage before a course
coBefParser :: Parsec.Parsec String () Req
coBefParser = do
    Parsec.spaces
    grade <- Parsec.try percentParser
    Parsec.manyTill (Parsec.anyChar) (Parsec.try (Parsec.lookAhead (singleParser)))
    req <- singleParser
    return $ GRADE grade req

-- parse for cutoff percentage after a course
coAftParser :: Parsec.Parsec String () Req
coAftParser = do
    Parsec.spaces
    req <- singleParser
    -- need to make it noneOf ,/andANDorOR()
    Parsec.manyTill (Parsec.try (Parsec.noneOf ",/()")) (Parsec.try (Parsec.lookAhead (Parsec.try andorParser <|> orParser)))
    grade <- percentParser
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
courseParser = Parsec.try (parParser) <|> (Parsec.try cutoffParser <|> singleParser)

-- parse for reqs separated by / "or"
orParser :: Parsec.Parsec String () Req
orParser = do
    tmp <- Parsec.sepBy (courseParser) (orSeparator)
    return $ OR tmp

-- parse for reqs separated by , "and"
andorParser :: Parsec.Parsec String () Req
andorParser = Parsec.try (fromParser) <|> (do
    tmp <- Parsec.sepBy (Parsec.try orParser <|> cutoffParser) (andSeparator)
    return $ AND tmp)

-- parse for reqs in "from" format
fromParser :: Parsec.Parsec String () Req
fromParser = do 
    fces <- fcesParser
    Parsec.manyTill (Parsec.letter) fromSeparator
    Parsec.manyTill (Parsec.anyChar) (Parsec.try (Parsec.lookAhead (courseParser)))
    req <- andorParser
    return $ FROM fces req

-- -- parse for reqs separated by ; "and?"
-- semcolParser :: Parsec.Parsec String () Req
-- semcolParser = do

-- *WebParsing.ReqParser> c = "csc148h1 (99%)  or csc207H1"
-- *WebParsing.ReqParser> parse andorParser   c
-- Right ((csc148h1 with a minimum grade of 99%)/csc207H1)
-- *WebParsing.ReqParser> c = "csc148h1   or 80% in csc207H1"
-- *WebParsing.ReqParser> parse andorParser   c
-- Left "(source)" (line 1, column 19):
-- unexpected "i"
-- expecting space, "/", "OR", "Or" or "or"
-- *WebParsing.ReqParser> c = "csc148h1   , 80% in csc207H1"
-- *WebParsing.ReqParser> parse andorParser   c
-- Right (csc148h1,(csc207H1 with a minimum grade of 80%))


    -- [x] fix cutoffParser
    -- [X] cutoffParser can now deal with cutoffs BEFORE a req
    -- [X] integrate cutoffParser into recursive structure
    -- [X] get andorParser to parse consecutive cutoffs
    -- [X] works with english, added more functionality to separators
    -- [X] fix fromParser to handle text between from and course req
    -- [X] Fix fromSeparator; FROM ISNT EVERYWHERE, BUT FCES IS.
    -- [X] Make cutoffParser work with english...
    -- [] cutoff parser for letter grades
    -- CSC165H1/CSC236H1/CSC240H1 (with a minimum grade of 60%), 
    -- [] CSC436H1/(CSC336H1 (75%))", nested cutoff
    -- [] create Group Type
    -- [] year (Done by Christine)
