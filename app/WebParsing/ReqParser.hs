{-# LANGUAGE FlexibleContexts #-}

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)
parse rule text = Parsec.parse rule "(source)" text


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
courseParser :: Parsec.Parsec String () String
-- alphanumeric? or is it already just strings, 8 characters.
-- 3 letters (not case sensitive), 3 digits, 1 letter, 1 digit
-- take any spaces before and after?
courseParser = do
    Parsec.spaces
    -- with no spaces, we expect 3 letters, 3 digits, and (h/H/y/Y)1
    code <- Parsec.count 3 Parsec.letter
    num <- Parsec.count 3 Parsec.digit
    sess <- Parsec.count 2 Parsec.alphaNum
    Parsec.spaces
    return (code++num++sess)

-- more lenient course parser
courseParser2 :: Parsec.Parsec String () String
-- alphanumeric, 8 characters alphanumeric?
courseParser2 = do
    Parsec.spaces
    course <- Parsec.count 8 Parsec.alphaNum
    Parsec.spaces
    return (course)

-- csc263/csc265, MAT235/Mat237/Mat257
-- PRECONDITION: '/' infix binary OP => will always have "courses" on both sides
--                => we will always have "course" followed by (/ "course")s
-- POSTCONDITION: returns list of "courses" that were separated by '/'
orParser1 :: Parsec.Parsec String () [String]
-- look for whitespaces, /, course, whitespaces. Repeat.
-- more efficiently, separate by optional whitespaces with '/'
orParser1 = Parsec.many $ do
    course <- courseParser
    Parsec.eof <|> orSeparator
    return course

-- lenient orParser, will simply parse by / and return list of "reqs"
orParser2 :: Parsec.Parsec String () [String]
-- look for whitespaces, /, course, whitespaces. Repeat.
-- more efficiently, separate by optional whitespaces with '/'
orParser2 = Parsec.sepBy (Parsec.many1 (Parsec.alphaNum <|> Parsec.oneOf ", ()")) (Parsec.char '/')

-- mat240,csc263/csc265, MAT235/Mat237/Mat257
-- PRECONDITION: ',' infix binary OP => will always have "courses" on both sides
--                => we will always have "course" followed by (, "course")s
-- POSTCONDITION: returns list of "courses" that were separated by ','
andParser1 :: Parsec.Parsec String () [String]
andParser1 = Parsec.many $ do
    course <- courseParser
    Parsec.eof <|> andSeparator
    return course

-- lenient orParser, will simply parse by , and return list of "reqs"
andParser2 :: Parsec.Parsec String () [String]
andParser2 = Parsec.sepBy (Parsec.many1 (Parsec.alphaNum <|> Parsec.oneOf "/ ()")) (Parsec.char ',')

-- parse by parantheses, treat everything inside paranthesis as one "req"
parParser1 :: Parsec.Parsec String () [String]

-- csc263/csc265, MAT235/Mat237/Mat257
-- PRECONDITION: '/' ',' infix binary OPs => will always have "courses" on both sides
-- POSTCONDITION: returns list of list of "courses". Depth 0 by ','' . Depth 1 by '/'.
andorParser :: Parsec.Parsec String () [Either Parsec.ParseError [String]]
andorParser = do
    andParsed <- andParser2
    let tmp = map (parse orParser2) andParsed
    return tmp


-- IDEA: within parParser, store all chunks of Reqs while parsing Req within parantheses until EOF then call
--       andorParser on all paranthesized Reqs. then call andorParser on 




-- adding functionality to andorParser to let it parse valid courses as well.
-- may be unnecessary since we can assume courses have no typo
-- might be more realistic to NOT expect perfectly formatted course codes due to inconsistencies in html.
--andorParser2 :: Parsec.Parsec String () [Either Parsec.ParseError [String]]
--andorParser2 = do
--    andParsed <- andParser2
--    let tmp = map (parse orParser2) andParsed
--    return map (map (parse courseParser2)) tmp

-- recursion comes in with ()
-- (mat136, mat136)
-- parse outer bracket, call lvl 3 then parse closing
-- parsec sepby

-- create parser for AND, OR, ANDOR, PARANTHESES, FROM..
-- MUST MAKE SURE TYPES MATCH UP..
-- AM I TRYING TO DO TOO MUCH IN ONE PARSER.. THINK RECURSIVELY.
-- 
-- break down by precedence and try different parsers and merge..?
