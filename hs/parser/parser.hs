module Main where

import Text.XML.HaXml
import Text.XML.HaXml.ByteStringPP
import Text.XML.HaXml.Wrappers
import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators
import Control.Monad.IO.Class  (liftIO)
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
          --let c = head $ map contentElem $ parseDocument z y
          --print $ map tagTextContent $ parseDocument z y
          --print $ getName c
          --print $ map (\x -> map convertAttributeToTuple x) $ map getAttrs $ map contentElem $ parseDocument z y
          --print $ map convertAttributeToTuple $ getAttrs c 
          --print $ map convertAttributeToTuple $ getAttrs $ head $ map contentElem $ parseDocument svg y
          --print $ parseDocument ch y
          --print $ map (\x -> parseContent (tag "path") x) $ parseDocument ch y
          --print $ getAttribute "x" $ head $ parseDocument z y
          --print $ map (getAttribute "id") $ parseLevel $ getRoot y
          print $ parseLevel $ getRoot y
--          print $ getAttrs $ contentElem $ head $ drop 9 $  getChildren $ getRoot y


parseLevel :: Content i -> (Float, Float)--[Content i]
parseLevel content3 = do
  -- Get Rects
  let rects = parseContent (tag "rect") content3
  -- Get Children
  let children = getChildren content3
  --let x = foldl (++) [] (map parseLevel children)
  let transform = getAttribute "transform" content3
  let x = if null transform then (0,0) else parseTransform transform
  let childrenTransformX = foldl (+) 0 (map fst $ map parseLevel $ getChildren content3)
  let childrenTransformY = foldl (+) 0 (map snd $ map parseLevel $ getChildren content3)
  (fst x + childrenTransformX, snd x + childrenTransformY)

getRoot :: Document i -> Content i
getRoot doc = head $ parseDocument (tag "svg") doc

filterAttrVal :: [Attribute] -> String -> String
filterAttrVal attrs attrName = snd $ head $ filter (\x -> snd x == attrName) $ map convertAttributeToTuple attrs

-- | Applys a CFilter to a Document and produces a list of the Content filtered 
-- by the CFilter.
parseDocument :: CFilter i -> Document i -> [Content i]
parseDocument filter (Document p s e m) = filter (CElem e undefined)

parseContent :: CFilter i -> Content i -> [Content i]
parseContent filter contenta = filter contenta

-- | Gets the tag name of an Element.
getName :: Element s -> String
getName (Elem a _ _) = printableName a

-- | Gets the list of Attributes of an Element.
getAttrs :: Element s -> [Attribute]
getAttrs (Elem _ b _) = b

-- | Gets an Attribute's name.
getAttrName :: Attribute -> String
getAttrName ((a,b)) = printableName a 

-- | Gets an Attribute's value.
getAttrVal :: Attribute -> String
getAttrVal ((a,b)) = show b

-- | Converts an Attribute into a more parsable form.
convertAttributeToTuple :: Attribute -> (String, String)
convertAttributeToTuple at = (getAttrName at, getAttrVal at)

getChildren :: Content i -> [Content i]
getChildren content1 = parseContent (path [children]) content1

isRect :: Content i -> Bool
isRect content1 = (getName $ contentElem content1) == "rect"

isPath :: Content i -> Bool
isPath content2 = (getName $ contentElem content2) == "path"

isText :: Content i -> Bool
isText content = (getName $ contentElem content) == "text"

getAttribute :: String -> Content i -> String
getAttribute attr (CElem content2 undefined) = 
  let x = filter (\x -> (getAttrName x) == attr) $ getAttrs content2
  in
  if null $ x
  then ""
  else getAttrVal $ head x
getAttribute _ _ = ""

parseTransform :: String -> (Float, Float) --(Rational, Rational)
parseTransform transform = do
    let commaPos = getComma 0 $ drop 9 transform
    let xPos = read $ drop 1 $ take commaPos $ drop 9 transform :: Float
    let yPos = read $ init $ drop (commaPos + 1) $ drop 9 transform :: Float
    (xPos, yPos)

getComma :: Int -> String -> Int
getComma accum x = if (head x) == ',' then accum else getComma (accum + 1) (tail x)

data Graph =
    Graph { 
            gId :: Int,
            title :: String
          } deriving (Show)

data Rect =
    Rect {
           width :: Rational,
           height :: Rational,
           xPos :: Rational,
           yPos :: Rational
         } deriving (Show)

data Text =
    Text {}

data Path =
    Path {}