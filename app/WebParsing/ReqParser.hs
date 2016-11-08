{-# LANGUAGE OverloadedStrings #-}
module WebParsing.ReqParser
    ( courseParser
    , courseParser2
    , orSeparator
    , andSeparator
    , lpSeparator
    , rpSeparator
    , fromSeparator
    , andParser
    , andParser2
    , orParser
    , orParser2
    , andorParser
    ) where

import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import qualified Data.Text as T
import Text.Parsec.String (Parser)

-- I import qualified so that it's clear which
-- functions are from the parsec library:
import qualified Text.Parsec as Parsec

-- I am the choice and optional error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)

-- alias parseTest for more concise usage in my examples:
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




-- parse "courses" separated by /
-- csc263/csc265, MAT235/Mat237/Mat257
-- PRECONDITION: '/' infix binary OP => will always have "courses" on both sides
--                => we will always have "course" followed by (/ "course")s
-- POSTCONDITION: returns list of "courses" that were separated by '/'
orParser :: Parsec.Parsec String () [String]
-- look for whitespaces, /, course, whitespaces. Repeat.
-- more efficiently, separate by optional whitespaces with '/'
orParser = Parsec.many $ do
    course <- courseParser
    Parsec.eof <|> orSeparator
    return course

-- parse "courses" separated by /
-- lenient orParser, will simply parse by / and return list of "reqs"
orParser2 :: Parsec.Parsec String () [String]
-- look for whitespaces, /, course, whitespaces. Repeat.
-- more efficiently, separate by optional whitespaces with '/'
orParser2 = Parsec.sepBy (Parsec.many1 (Parsec.alphaNum <|> Parsec.oneOf ", ()")) (Parsec.char '/')



-- mat240,csc263/csc265, MAT235/Mat237/Mat257
-- PRECONDITION: ',' infix binary OP => will always have "courses" on both sides
--                => we will always have "course" followed by (, "course")s
-- POSTCONDITION: returns list of "courses" that were separated by ','
andParser :: Parsec.Parsec String () [String]
andParser = Parsec.many $ do
    course <- courseParser
    Parsec.eof <|> andSeparator
    return course

-- lenient orParser, will simply parse by , and return list of "reqs"
andParser2 :: Parsec.Parsec String () [String]
andParser2 = Parsec.sepBy (Parsec.many1 (Parsec.alphaNum <|> Parsec.oneOf "/ ()")) (Parsec.char ',')


-- parse "courses" separated by , then by /
-- csc263/csc265, MAT235/Mat237/Mat257
-- PRECONDITION: '/' ',' infix binary OPs => will always have "courses" on both sides
-- POSTCONDITION: returns list of list of "courses". Depth 0 ','' . Depth 1 '/'.
andorParser :: Parsec.Parsec String () [[String]]
andorParser = 



-- recursion comes in with ()
-- (mat136, mat136)
-- parse outer bracket, call lvl 3 then parse closing
-- parsec sepby


