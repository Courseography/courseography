{-# LANGUAGE FlexibleContexts #-}
module WebParsing.ReqParser
    (andorParser) where

import qualified Text.Parsec as Parsec
import Text.Parsec.String
import Text.Parsec ((<|>))
import Control.Monad.Identity (Identity)
import qualified Data.String as S
import qualified Data.List as L
import Database.Requirement
parse rule text = Parsec.parse rule "(source)" text

-- define separators for "from"
fromSeparator :: Parsec.Parsec String () ()
fromSeparator = Parsec.spaces >> Parsec.try (Parsec.string "FROM" 
             <|> Parsec.string "from" <|> Parsec.string "From") >> Parsec.spaces

lpSeparator :: Parsec.Parsec String () ()
lpSeparator = Parsec.spaces >> Parsec.char '(' >> Parsec.spaces

rpSeparator :: Parsec.Parsec String () ()
rpSeparator = Parsec.spaces >> Parsec.char ')' >> Parsec.spaces

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
    return [int,float]

intParserLenient :: Parsec.Parsec String () String
intParserLenient = do
    Parsec.try (Parsec.manyTill Parsec.anyChar (Parsec.try 
               (Parsec.notFollowedBy (Parsec.noneOf ['0'..'9']))))
    fces <- intParser
    return fces

intParser :: Parsec.Parsec String () String
intParser = do
    Parsec.spaces
    fces <- Parsec.digit
    return [fces]

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
    if length_list tmp == 1
    then
        case tmp of
        [x] -> return x
    else
        return $ OR tmp

-- parse for reqs separated by , "and"
andorParser :: Parsec.Parsec String () Req
andorParser = do
    tmp <- Parsec.sepBy (orParser) (Parsec.char ',')
    if length_list tmp == 1
    then
        case tmp of
        [x] -> return x
    else
        return $ AND tmp

-- parse for reqs within parantheses
parParser :: Parsec.Parsec String () Req
parParser = do
    lpSeparator
    req <- andorParser
    rpSeparator
    return $ PAR req

-- parse for reqs in "from" format
fromParser :: Parsec.Parsec String () Req
fromParser = do 
    fces <- intParserLenient <|> floatParserLenient
    Parsec.try (Parsec.manyTill Parsec.anyChar (Parsec.try fromSeparator))
    reqs <- andorParser
    return $ FROM fces reqs