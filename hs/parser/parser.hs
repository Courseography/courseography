module Main where

import Text.XML.HaXml
import Text.XML.HaXml.ByteStringPP
import Text.XML.HaXml.Wrappers

main :: IO ()
main = do x <- readFile "../../res/graphs/graph_regions.svg"
          let y = xmlParse "output.error" x
          let z = path [tag "svg"]
          print $ onContent z y

