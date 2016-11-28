{-# LANGUAGE FlexibleContexts #-}

import qualified Text.Parsec as Parsec
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
    show (AND reqs) = "(" ++ (L.intercalate "," $ map show reqs) ++ ")"
    show (OR reqs) = "(" ++ (L.intercalate "/" $ map show reqs) ++ ")"
    show (FROM fces reqs) =  fces ++ " FCE(s) from:\n" ++ show reqs
    show (GRADE grade reqs) =  "(" ++ show reqs ++ " with a minimum grade of " ++ grade ++ "%)"

-- define separators for "from"
fromSeparator :: Parsec.Parsec String () ()
fromSeparator = Parsec.spaces >> Parsec.try (Parsec.string "FROM" 
             <|> Parsec.string "from" <|> Parsec.string "From") >> Parsec.spaces

gradeSeparator :: Parsec.Parsec String () ()
gradeSeparator = Parsec.spaces >> Parsec.try (Parsec.string "MINIMUM" 
             <|> Parsec.string "Minimum" <|> Parsec.string "minimum") >> Parsec.spaces

cutoffSeparator :: Parsec.Parsec String () ()
cutoffSeparator = Parsec.spaces >> Parsec.digit >> Parsec.digit >> Parsec.char '%' >> Parsec.spaces

cutoffSeparator1 :: Parsec.Parsec String () ()
cutoffSeparator1 = Parsec.spaces >> Parsec.char '(' >> Parsec.digit >> Parsec.digit >> Parsec.char '%' >> Parsec.char ')' >> Parsec.spaces

lpSeparator :: Parsec.Parsec String () ()
lpSeparator = Parsec.spaces >> Parsec.char '(' >> Parsec.spaces

rpSeparator :: Parsec.Parsec String () ()
rpSeparator = Parsec.spaces >> Parsec.char ')' >> Parsec.spaces

orSeparator :: Parsec.Parsec String () ()
orSeparator = Parsec.spaces >> (Parsec.string "," <|> Parsec.string "AND" <|> Parsec.string "And" <|> Parsec.string "and") >> Parsec.spaces

andSeparator :: Parsec.Parsec String () ()
andSeparator = Parsec.spaces >> (Parsec.string "/" <|> Parsec.string "OR" <|> Parsec.string "Or" <|> Parsec.string "or") >> Parsec.spaces

-- helper to find length of list of reqs
length_list :: [Req] -> Int 
length_list [] = 0
length_list (x:xs) =1 + length_list xs

-- "5.0 fces from..." "3 full course from" "2.5 full course from"
-- Hard-coded for now since all FCEs are of form _._ with anything before
floatParserLenient :: Parsec.Parsec String () String
floatParserLenient = do
    Parsec.try (Parsec.manyTill Parsec.anyChar (Parsec.try 
               (Parsec.notFollowedBy (Parsec.noneOf ['0'..'9']))))
    fces <- floatParser
    return fces

floatParser :: Parsec.Parsec String () String
floatParser = do
    Parsec.spaces
    int <- Parsec.digit
    Parsec.char '.'
    float <- Parsec.digit
    return [int,'.',float]

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

cutoffParser :: Parsec.Parsec String () String
cutoffParser = do
    fces <- Parsec.count 2 Parsec.digit
    Parsec.char '%'
    return fces

cutoffParser1 :: Parsec.Parsec String () String
cutoffParser1 = do
    Parsec.char '('
    fces <- Parsec.count 2 Parsec.digit
    Parsec.char '%'
    Parsec.char ')'
    return fces

cutoffParserLenient :: Parsec.Parsec String () String
cutoffParserLenient = do
    Parsec.manyTill Parsec.anyChar (Parsec.try (Parsec.lookAhead (cutoffParser <|> cutoffParser1)))
    fces <- cutoffParser <|> cutoffParser1
    return fces

-- parse for single course OR req within parantheses
courseParser :: Parsec.Parsec String () Req
courseParser = Parsec.try (do
    Parsec.spaces
    -- with no spaces, we expect 3 letters, 3 digits, and (h/H/y/Y)1
    code <- Parsec.count 3 Parsec.letter
    num <- Parsec.count 3 Parsec.digit
    sess <- Parsec.count 2 Parsec.alphaNum
    Parsec.spaces
    return $ J (code++num++sess)) <|> (parParser)

-- parse for reqs separated by / "or"
orParser :: Parsec.Parsec String () Req
orParser = do
    tmp <- Parsec.sepBy (courseParser) (Parsec.char '/')
    return $ OR tmp

-- parse for reqs separated by , "and"
andorParser :: Parsec.Parsec String () Req
andorParser = do
    tmp <- Parsec.sepBy (orParser) (Parsec.char ',')
    return $ AND tmp

-- parse for reqs within parantheses
parParser :: Parsec.Parsec String () Req
parParser = do
    lpSeparator
    req <- andorParser
    rpSeparator
    return req

-- parse for reqs in "from" format
fromParser :: Parsec.Parsec String () Req
fromParser = do 
    fces <- intParserLenient <|> floatParserLenient
    Parsec.try (Parsec.manyTill Parsec.anyChar (Parsec.try fromSeparator))
    reqs <- andorParser
    return $ FROM fces reqs

-- high-level gradeParser
gradeParser :: Parsec.Parsec String () Req
gradeParser = Parsec.try gradeParser2 <|> gradeParser1

-- parse for "minimum" grade cutoff of form REQ(--%)
gradeParser1 :: Parsec.Parsec String () Req
gradeParser1 = do
    req <- Parsec.manyTill (andorParser) (Parsec.try (Parsec.lookAhead (cutoffParser <|> cutoffParser1)))
    grade <- cutoffParser <|> cutoffParser1
    case req of
        [x] -> return $ GRADE grade x

-- parse for "minimum" grade cutoff of form REQ (with a minimum ___ --%)
gradeParser2 :: Parsec.Parsec String () Req
gradeParser2 = do
    req <- Parsec.manyTill (andorParser) (Parsec.try (Parsec.lookAhead (Parsec.string "with a minimum")))
    grade <- cutoffParserLenient
    case req of
        [x] -> return $ GRADE grade x

-- TODO: error msg
---- NEED TO MAKE IMPORTS AND EXPORTS CONSISTENT
----- MUST TEST WITH REPL