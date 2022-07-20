{-|
Description: ReqParser in a Main module so it can be compiled into an
executable and tested externally.
-}

module Main (main) where

import WebParsing.ReqParser (parseReqs)

main :: IO ()
main = do
    reqString <- getLine
    print $ parseReqs reqString
