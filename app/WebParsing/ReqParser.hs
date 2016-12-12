{-# LANGUAGE FlexibleContexts #-}
module WebParsing.ReqParser
    (reqParser) where

import qualified Text.Parsec as Parsec
import Text.Parsec.String
import Text.Parsec ((<|>))
import Control.Monad.Identity (Identity)
import qualified Data.String as S
import qualified Data.List as L
import Database.Requirement
parse rule text = Parsec.parse rule "(source)" text

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
    junk1 <- Parsec.manyTill (Parsec.anyChar) (Parsec.try (Parsec.lookAhead (singleParser)))
    reqs <- singleParser
    junk2 <- Parsec.manyTill Parsec.anyChar (Parsec.try (Parsec.notFollowedBy (Parsec.try
                                             (Parsec.noneOf ",/():;\r\n"))))
    return $ JUNK junk1 reqs junk2

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
    Parsec.manyTill (Parsec.anyChar) (Parsec.try (Parsec.lookAhead (singleParser)))
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

    -- [] look for more conrner cases where separators are in english between
    --    complex reqs.
    -- [] year (Done by Christine)
    -- [] create Group Type
