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
fromSeparator = Parsec.spaces >> Parsec.try (Parsec.string "FROM" 
             <|> Parsec.string "from" <|> Parsec.string "From") >> Parsec.spaces

lpSeparator :: Parsec.Parsec String () ()
lpSeparator = Parsec.spaces >> Parsec.char '(' >> Parsec.spaces

rpSeparator :: Parsec.Parsec String () ()
rpSeparator = Parsec.spaces >> Parsec.char ')' >> Parsec.spaces

orSeparator :: Parsec.Parsec String () ()
orSeparator = Parsec.spaces >> (Parsec.string "/" <|> Parsec.string "OR" <|> Parsec.string "Or" <|> Parsec.string "or") >> Parsec.spaces

andSeparator :: Parsec.Parsec String () ()
andSeparator = Parsec.spaces >> (Parsec.string "," <|> Parsec.string "AND" <|> Parsec.string "And" <|> Parsec.string "and") >> Parsec.spaces


floatParser :: Parsec.Parsec String () String
floatParser = do
    Parsec.spaces
    int <- Parsec.digit
    Parsec.char '.'
    float <- Parsec.digit
    return [int,'.',float]

floatParserLenient :: Parsec.Parsec String () String
floatParserLenient = do
    Parsec.try (Parsec.manyTill Parsec.anyChar (Parsec.try 
               (Parsec.notFollowedBy (Parsec.noneOf ['0'..'9']))))
    fces <- floatParser
    return fces

intParser :: Parsec.Parsec String () String
intParser = do
    Parsec.spaces
    fces <- Parsec.digit
    return [fces]

intParserLenient :: Parsec.Parsec String () String
intParserLenient = do
    Parsec.try (Parsec.manyTill Parsec.anyChar (Parsec.try 
               (Parsec.notFollowedBy (Parsec.noneOf ['0'..'9']))))
    fces <- intParser
    return fces

percentParser :: Parsec.Parsec String () String
percentParser = do
    Parsec.spaces
    Parsec.many (Parsec.noneOf ['0'..'9'])
    fces <- Parsec.count 2 Parsec.digit
    Parsec.char '%'
    return fces

-- parse for cutoff percentage before a course
coBefParser :: Parsec.Parsec String () Req
coBefParser = do
    grade <- percentParser
    Parsec.manyTill (Parsec.anyChar) (Parsec.try (Parsec.lookAhead (singleParser)))
    req <- singleParser
    return $ GRADE grade req

-- parse for cutoff percentage after a course
coAftParser :: Parsec.Parsec String () Req
coAftParser = do
    req <- singleParser
    grade <- percentParser
    return $ GRADE grade req

-- parse for courses with cutoffs
cutoffParser :: Parsec.Parsec String () Req
cutoffParser = Parsec.try (coAftParser) <|> (coBefParser)

-- parse for reqs within parantheses
parParser :: Parsec.Parsec String () Req
parParser = do
    lpSeparator
    req <- andorParser
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
courseParser = Parsec.try (cutoffParser) <|> (parParser) <|> (singleParser)

-- parse for reqs separated by / "or"
orParser :: Parsec.Parsec String () Req
orParser = do
    tmp <- Parsec.sepBy (courseParser) (orSeparator)
    return $ OR tmp

-- parse for reqs separated by , "and"
andorParser :: Parsec.Parsec String () Req
andorParser = Parsec.try (fromParser) <|> (do
    tmp <- Parsec.sepBy (orParser) (andSeparator)
    return $ AND tmp)

-- parse for reqs in "from" format
fromParser :: Parsec.Parsec String () Req
fromParser = do 
    fces <- floatParserLenient <|> intParserLenient
    Parsec.try (Parsec.manyTill Parsec.anyChar (Parsec.try fromSeparator))
    reqs <- andorParser
    return $ FROM fces reqs

    -- [x] fix cutoffParser
    -- [X] cutoffParser can now deal with cutoffs BEFORE a req
    -- [X] integrate cutoffParser into recursive structure
    -- [] get andorParser to parse consecutive coBEFs; it works for consecutive coAFTs
    -- [] create Group Type
    -- [] year (Done by Christine)