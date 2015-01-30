module Main where

import Text.XML.HaXml
import Text.XML.HaXml.ByteStringPP
import Text.XML.HaXml.Wrappers
import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators
import Text.XML.HaXml.Namespaces

main :: IO ()
main = do x <- readFile "../../res/graphs/graph_regions.svg"
          let at = AttValue $ [Left "913.45227"]
          let y = xmlParse "output.error" x
          let z = deep $ attrval (N "x", at)
          print $ parseContent z y

-- | I took this from onContent, and modified it quite a bit.
parseContent :: CFilter i -> Document i -> [Content i]
parseContent filter (Document p s e m) = filter (CElem e undefined)