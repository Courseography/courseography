module Main where

import Text.XML.HaXml
import Text.XML.HaXml.ByteStringPP
import Text.XML.HaXml.Wrappers
import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators
import Text.XML.HaXml.Util
import Text.XML.HaXml.XmlContent.Parser
import Text.XML.HaXml.Namespaces

main :: IO ()
main = do x <- readFile "../../res/graphs/graph_regions.svg"
          let at = AttValue $ [Left "913.45227"]
          let y = xmlParse "output.error" x
          let a = attrval (N "x", at)
          let z = deep $ tag "text"
          let c = head $ map contentElem $ parseContent z y
          --print $ parseContent z y
          print $ map tagTextContent $ parseContent z y
          print $ getName c
          print $ map (\x -> map convertAttributeToTuple x) $ map getAttrs $ map contentElem $ parseContent z y
          print $ map convertAttributeToTuple $ getAttrs c

-- | I took this from onContent, and modified it quite a bit.
parseContent :: CFilter i -> Document i -> [Content i]
parseContent filter (Document p s e m) = filter (CElem e undefined)

getName :: Element s -> String
getName (Elem a _ _) = printableName a

getAttrs :: Element s -> [Attribute]
getAttrs (Elem a b _) = b

getAttrVals :: Element s -> [Attribute]
getAttrVals el = getAttrs el

getAttrName :: Attribute -> String
getAttrName ((a,b)) = printableName a 

getAttrVal :: Attribute -> String
getAttrVal ((a,b)) = show b

convertAttributeToTuple :: Attribute -> (String, String)
convertAttributeToTuple at = (getAttrName at, getAttrVal at)

