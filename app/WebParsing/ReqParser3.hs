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

-- parse for single course OR req within parantheses
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

-- parse for reqs separated by / "or"
orParser :: Parsec.Parsec String () Req
orParser = do
    tmp <- Parsec.sepBy (courseParser) (Parsec.char '/')
    return $ OR tmp

orParser1 :: Parsec.Parsec String () Req
orParser1 = do
    tmp <- orParser
    case tmp of
        Right parsed ->
    	    case parsed of
    		OR [J x] -> J x
    		OR [OR x] -> OR x
    		OR [AND x] -> AND x
    		OR xs -> OR xs
        Left _ -> tmp

-- parse for reqs separated by , "and"
andorParser :: Parsec.Parsec String () Req
andorParser = do
    tmp <- Parsec.sepBy (orParser1) (Parsec.char ',')
    return $ AND tmp

andorParser1 :: Parsec.Parsec String () Req
andorParser1 = do
    tmp <- andorParser
    case tmp of
        Right parsed ->
            case parsed of
            AND [J x] -> J x
            AND [OR x] -> OR x
            AND [AND x] -> AND x
            AND xs -> AND xs
        Left _ -> tmp

parParser :: Parsec.Parsec String () Req
parParser = do
    lpSeparator
    req <- andorParser1
    rpSeparator
    return req

-- case check? if highest level is of form AND[OR xs] or OR[AND xs], return OR xs or AND xs, respectively
-- do this recursively so that we get rid of the bottom level ORs and ANDs??


-- TODO: error msg
---- display
---- recursive structure
---- unformatted course
---- FROM value constructor