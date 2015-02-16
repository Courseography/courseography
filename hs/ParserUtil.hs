module ParserUtil where

import Text.XML.HaXml
import Text.XML.HaXml.ByteStringPP
import Text.XML.HaXml.Wrappers
import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators
import Control.Monad.IO.Class  (liftIO)
import Text.XML.HaXml.Util
import Text.XML.HaXml.XmlContent.Parser
import qualified Data.Conduit.List as CL
import Database.Persist
import Database.Persist.Sqlite
import Text.XML.HaXml.Namespaces
import Data.Conduit
import Data.List.Split
import Data.List
import Data.Text as T (pack, unpack)
import Tables
import JsonParser
import SVGTypes

-- | Gets the root element of the document.
getRoot :: Document i -> Content i
getRoot doc = head $ parseDocument (tag "svg") doc

-- | Gets a style attribute from a style String.
getStyleAttr :: String -> String -> String
getStyleAttr attr style =
    drop (length attr + 1) $
    head $ filter
            (\x -> take (length attr + 1) x == (attr ++ ":"))
            (splitOn ";" style) ++ [""]

-- | Gets a style attribute from a style String. If the style attribute is "",
-- then this function defaults to the previous style attribute, 'parent'.
getNewStyleAttr :: String -> String -> String -> String
getNewStyleAttr newStyle attr parent = 
    if null (getStyleAttr attr newStyle)
    then parent
    else getStyleAttr attr newStyle

-- | Converts a tuple of Float to a tuple of Rational.
convertFloatTupToRationalTup :: (Float, Float) -> (Rational, Rational)
convertFloatTupToRationalTup tup = (toRational (fst tup), toRational (snd tup))

-- | Applys a CFilter to a Document and produces a list of Content filtered 
-- by the CFilter.
parseDocument :: CFilter i -> Document i -> [Content i]
parseDocument filter (Document p s e m) = filter (CElem e undefined)

-- | Applys a CFilter to a Content and produces a list of Content filtered by the
-- CFilter.
parseContent :: CFilter i -> Content i -> [Content i]
parseContent filter = filter

-- | Gets the tag name of an Element.
getName :: Content i -> String
getName (CElem (Elem a _ _) _) = printableName a
getName _ = ""

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

-- | Gets the children of the current node.
getChildren :: Content i -> [Content i]
getChildren = parseContent (path [children])

-- | Gets the value of the attribute with the corresponding key.
getAttribute :: String -> Content i -> String
getAttribute attr (CElem content undefined) = 
    let x = filter (\x -> getAttrName x == attr) $ getAttrs content
    in if null x
       then ""
       else getAttrVal $ head x
getAttribute _ _ = ""

-- | Parses a transform String into a tuple of Float.
parseTransform :: String -> (Float, Float)
parseTransform transform = 
    do let commaPos = getComma 0 $ drop 9 transform
       let xPos = read $ drop 1 $ take commaPos $ drop 9 transform :: Float
       let yPos = read $ init $ drop (commaPos + 1) $ drop 9 transform :: Float
       (xPos, yPos)

-- | Gets the location of a comma in a string.
-- NOTE: Can probably be replaced by Data.List.Split.
getComma :: Int -> String -> Int
getComma accum x = if head x == ',' then accum else getComma (accum + 1) (tail x)

-- | Parses a path's `d` attribute.
parsePathD :: String -> [(Float, Float)]--[(Rational, Rational)]
parsePathD d = 
    if head d == 'm'
    then foldCoordsRel $ filter (\x -> length x > 1) $ map (splitOn ",") $ splitOn " " d
    else processAbsCoords $ filter (\x -> length x > 1) $ map (splitOn ",") $ splitOn " " d

-- | Converts a relative coordinate structure into an absolute one.
foldCoordsRel :: [[String]] -> [(Float, Float)]
foldCoordsRel dCoords =
    tail $
    foldl (\x y -> x ++ [addTuples (convertToFloatTuple y) (last x)])
          [(0,0)]
          dCoords

-- | Converts a relative coordinate structure into an absolute one.
processAbsCoords :: [[String]] -> [(Float, Float)]
processAbsCoords = map convertToFloatTuple

-- | Converts a list of String of length 2 into a tuple of Float.
convertToFloatTuple :: [String] -> (Float, Float)
convertToFloatTuple y = (read (head y) :: Float, read (last y) :: Float)

-- | Adds two tuples together.
addTuples :: (Float, Float) -> (Float, Float) -> (Float, Float)
addTuples tup1 tup2 = (fst tup1 + fst tup2, snd tup1 + snd tup2)

-- | Determines if a point intersects with a shape.
intersects :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Bool
intersects width height rx ry offset px py = do
    let dx = px - rx
    let dy = py - ry
    dx >= -1 * offset && dx <= width + offset && dy >= -1 * offset && dy <= height + offset;

-- | Removes the part of a string after the first forward slash.
dropSlash :: String -> String
dropSlash str = head $ splitOn "/" str