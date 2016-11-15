{-# LANGUAGE FlexibleContexts #-}

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)
import qualified Data.String as S
parse rule text = Parsec.parse rule "(source)" text

data ProgramReq = PRGREQ String [Req] deriving (Show)

-- | Returns a well formatted String representing a program requirement for specified program.
-- instance Show ProgramReq where
--     show (PRGREQ program reqs) = "Program Requirements for " ++ program ++ ":\n" ++ map show reqs


data CourseReq = CRSREQ String Req Req Req deriving (Show)

-- instance Show CourseReq where
--     show (CRSREQ course creq excl preq) = "Corequisites for " ++ course ++ ":\n" ++ show req ++ "\n"
--                                       ++ "Exclusions for " ++ course ++ ":\n" ++ show req ++ "\n"
--                                       ++ "Prerequisites for " ++ course ++ ":\n" ++ show req ++ "\n"


-- for now J seems to be most readable and convenient value constructor for satisfying rec structure.
data Req = J String | AND [Req] | OR [Req] | FROM Integer [Req] deriving (Show)

-- instance Show Req where
--     show (J course) = course
--     show (AND reqs) = S.intercalate "," $ map show reqs
--     show (OR reqs) = S.intercalate "/" $ map show reqs
--     show (FROM a x) =  show a ++ "FCE(s) from: (" ++ show x ++ ")"

-- define separators for "/"  ","  "("  ")"  "from"
orSeparator :: Parsec.Parsec String () ()
orSeparator = Parsec.spaces >> Parsec.char '/' >> Parsec.spaces

andSeparator :: Parsec.Parsec String () ()
andSeparator = Parsec.spaces >> Parsec.char ',' >> Parsec.spaces

lpSeparator :: Parsec.Parsec String () ()
lpSeparator = Parsec.spaces >> Parsec.char '(' >> Parsec.spaces

rpSeparator :: Parsec.Parsec String () ()
rpSeparator = Parsec.spaces >> Parsec.char ')' >> Parsec.spaces

fromSeparator :: Parsec.Parsec String () ()
fromSeparator = Parsec.spaces >> Parsec.oneOf "fromFrom" >> Parsec.spaces
-- potentially have one separator that returns accordingly?

-- csc263h1, mat157Y1, STA257H1
-- Parse a single course with spaces before and or after
courseParser :: Parsec.Parsec String () Req
-- alphanumeric? or is it already just strings, 8 characters.
-- 3 letters (not case sensitive), 3 digits, 1 letter, 1 digit
-- take any spaces before and after?
courseParser = (do
    Parsec.spaces
    -- with no spaces, we expect 3 letters, 3 digits, and (h/H/y/Y)1
    code <- Parsec.count 3 Parsec.letter
    num <- Parsec.count 3 Parsec.digit
    sess <- Parsec.count 2 Parsec.alphaNum
    Parsec.spaces
    return $ J (code++num++sess)) <|> (parParser)

-- more lenient course parser
-- courseParser2 :: Parsec.Parsec String () String
-- -- alphanumeric, 8 characters alphanumeric?
-- courseParser2 = do
--     Parsec.spaces
--     course <- Parsec.count 8 Parsec.alphaNum
--     Parsec.spaces
--     return (course)

-- courseParser3 :: String -> [String]
-- courseParser3 course = 
--     let parsedCourse = parse andParser2 course
--     in
--         case parsedCourse of
--             Right xs -> xs
--             Left _ -> [""]

-- csc263/csc265, MAT235/Mat237/Mat257
-- PRECONDITION: '/' infix binary OP => will always have "courses" on both sides
--                => we will always have "course" followed by (/ "course")s
-- POSTCONDITION: returns list of "courses" that were s     eparated by '/'
--orParser1 :: Parsec.Parsec String () [String]
-- look for whitespaces, /, course, whitespaces. Repeat.
-- more efficiently, separate by optional whitespaces with '/'
--orParser1 = Parsec.many $ do
  --  course <- courseParser
    --Parsec.eof <|> orSeparator
   -- return  course

-- lenient orParser, will simply parse by / and return list of "reqs"
orParser2 :: Parsec.Parsec String () Req
-- look for whitespaces, /, course, whitespaces. Repeat.
-- more efficiently, separate by optional whitespaces with '/'
orParser2 = do
    --tmp <- Parsec.sepBy (Parsec.many1 (Parsec.alphaNum <|> Parsec.oneOf ", ()")) (Parsec.char '/')
    tmp <- Parsec.sepBy (courseParser) (Parsec.char '/')
    return $ OR tmp

-- orParser3 :: String -> [String]
-- orParser3 req = 
--     let orParsed = parse orParser2 req
--     in
--         case orParsed of
--             Right xs -> xs
--             Left _ -> [""]


-- mat240,csc263/csc265, MAT235/Mat237/Mat257
-- PRECONDITION: ',' infix binary OP => will always have "courses" on both sides
--                => we will always have "course" followed by (, "course")s
-- POSTCONDITION: returns list of "courses" that were separated by ','
-- andParser1 :: Parsec.Parsec String () [String]
-- andParser1 = Parsec.many $ do
--     req <- courseParser
--     Parsec.eof <|> andSeparator
--     return req

-- lenient orParser, will simply parse by , and return list of "reqs"
andorParser2 :: Parsec.Parsec String () Req
andorParser2 = do
    --tmp <- Parsec.sepBy (Parsec.many1 (Parsec.alphaNum <|> Parsec.oneOf "/ ()")) (Parsec.char ',')
    tmp <- Parsec.sepBy (orParser2) (Parsec.char ',')
    return $ AND tmp

parParser :: Parsec.Parsec String () Req
parParser = do
    lpSeparator
    req <- andorParser2
    rpSeparator
    return req

-- uses andParser2
-- andParser3 :: String -> [String]
-- andParser3 req = 
--     let andParsed = parse andParser2 req
--     in
--         case andParsed of
--             Right xs -> xs
--             Left _ -> [""]


-- parse by parantheses, treat everything inside paranthesis as one "req"
--parParser1 :: Parsec.Parsec String () [String]

-- csc263/csc265, MAT235/Mat237/Mat257
-- PRECONDITION: '/' ',' infix binary OPs => will always have "courses" on both sides
-- POSTCONDITION: returns list of list of "courses". Depth 0 by ','' . Depth 1 by '/'.
-- andorParser :: Parsec.Parsec String () [Either Parsec.ParseError [String]]
-- andorParser = do
--     andParsed <- andParser2
--     let andorParsed = map (parse orParser2) andParsed
--     return andorParsed

-- adding functionality to andorParser to let it parse valid courses as well.
-- may be unnecessary since we can assume courses have no typo
-- might be more realistic to NOT expect perfectly formatted course codes due to inconsistencies in html.
-- andorParser2 :: String -> [[String]]
-- andorParser2 req = map (orParser3) $ andParser3 req

--reqParser :: String -> [[String]]

--lpParser1 :: Parsec.Parsec String () String
--lpParser1 = do
--    req <- Parsec.many1 (Parsec.alphaNum <|> Parsec.oneOf "/, )")
--    Parsec.eof <|> Parsec.char '('
--    return req

--rpParser1 :: Parsec.Parsec String () String
--rpParser1 = do
---    req <- Parsec.many1 (Parsec.alphaNum <|> Parsec.oneOf "/, (")
--    Parsec.eof <|> Parsec.char ')'
--    return req


-- parse everything by parantheses
-- left or right may be empty, meaning it's not in the middle of a req
--parParser1 :: Parsec.Parsec String () [[String]]
--parParser1 = Parsec.many $ do
--    left <- lpParser1
--    right <- rpParser1
--    return [left, right]


-- recursion comes in with ()
-- (mat136, mat136)
-- parse outer bracket, call lvl 3 then parse closing
-- parsec sepby

-- create parser for AND, OR, ANDOR, PARANTHESES, FROM..
-- MUST MAKE SURE TYPES MATCH UP..
-- AM I TRYING TO DO TOO MUCH IN ONE PARSER.. THINK RECURSIVELY.
-- 
-- break down by precedence and try different parsers and merge..?
