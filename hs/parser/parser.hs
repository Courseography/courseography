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
          let z = deep $ tag "rect"
          let svg = tag "svg"
          let ch = path [children]
          let c = head $ map contentElem $ parseDocument z y
          --print $ map tagTextContent $ parseDocument z y
          --print $ getName c
          --print $ map (\x -> map convertAttributeToTuple x) $ map getAttrs $ map contentElem $ parseDocument z y
          --print $ map convertAttributeToTuple $ getAttrs c 
          --print $ map convertAttributeToTuple $ getAttrs $ head $ map contentElem $ parseDocument svg y
          --print $ parseDocument ch y
          print $ map (\x -> parseContent (tag "path") x) $ parseDocument ch y
          print $ getRoot y

getRoot :: Document i -> Content i
getRoot doc = head $ parseDocument (tag "svg") doc

filterAttrVal :: [Attribute] -> String -> String
filterAttrVal attrs attrName = snd $ head $ filter (\x -> snd x == attrName) $ map convertAttributeToTuple attrs

-- | Applys a CFilter to a Document and produces a list of the Content filtered 
-- by the CFilter.
parseDocument :: CFilter i -> Document i -> [Content i]
parseDocument filter (Document p s e m) = filter (CElem e undefined)

parseContent :: CFilter i -> Content i -> [Content i]
parseContent filter cont = filter cont 

-- | Gets the tag name of an Element.
getName :: Element s -> String
getName (Elem a _ _) = printableName a

-- | Gets the list of Attributes of an Element.
getAttrs :: Element s -> [Attribute]
getAttrs (Elem a b _) = b

-- | Gets an Attribute's name.
getAttrName :: Attribute -> String
getAttrName ((a,b)) = printableName a 

-- | Gets an Attribute's value.
getAttrVal :: Attribute -> String
getAttrVal ((a,b)) = show b

-- | Converts an Attribute into a more parsable form.
convertAttributeToTuple :: Attribute -> (String, String)
convertAttributeToTuple at = (getAttrName at, getAttrVal at)

data Graph =
    Graph { 
            gId :: Int,
            title :: String
          } deriving (Show)

data Rect =
    Rect {
           rId :: Int,
           width :: Rational,
           height :: Rational,
           xPos :: Rational,
           yPos :: Rational,
           colour :: String
         } deriving (Show)

data Text =
    Text {}

data Path =
    Path {}