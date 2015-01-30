module Main where

import Text.XML.HaXml
import Text.XML.HaXml.ByteStringPP
import Text.XML.HaXml.Wrappers

main :: IO ()
main = do x <- readFile "../../res/graphs/graph_regions.svg"
          let y = xmlParse "output.error" x
          let z = path [children, tag "g", children, tag "path"]
          print $ parseContent z y

-- | I took this from onContent, and modified it quite a bit.
parseContent :: CFilter i -> Document i -> [Content i]
parseContent filter (Document p s e m) = filter (CElem e undefined)