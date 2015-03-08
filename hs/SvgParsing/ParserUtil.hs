module SvgParsing.ParserUtil where

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
import Data.List.Split hiding (startsWith)
import Data.List
import Data.Text as T (pack, unpack)
import Database.Tables
import Database.JsonParser
import SvgParsing.SVGTypes

-- | Gets the root element of the document.
getRoot :: Document i -> Content i
getRoot doc = head $ parseDocument (tag "svg") doc

-- | Gets a style attribute from a style String.
getStyleAttr :: String -> String -> String
getStyleAttr attr style =
    drop (length attr + 1) $
    head $ filter (isPrefixOf $ attr ++ ":") (splitOn ";" style) ++ [""]

-- | Gets a style attribute from a style String. If the style attribute is "",
-- then this function defaults to the previous style attribute, 'parent'.
getNewStyleAttr :: String -> String -> String -> String
getNewStyleAttr newStyle attr parentStyle
    | null newAttrStyle = parentStyle
    | otherwise = newAttrStyle
    where newAttrStyle = getStyleAttr attr newStyle

-- | Applys a CFilter to a Document and produces a list of Content filtered
-- by the CFilter.
parseDocument :: CFilter i -> Document i -> [Content i]
parseDocument filter (Document _ _ e _) = filter (CElem e undefined)

-- | Gets the tag name of an Element.
getName :: Content i -> String
getName (CElem (Elem a _ _) _) = printableName a
getName _ = ""

-- | Gets the list of Attributes of an Element.
getAttrs :: Element s -> [Attribute]
getAttrs (Elem _ b _) = b

-- | Gets an Attribute's name.
getAttrName :: Attribute -> String
getAttrName ((a,_)) = printableName a

-- | Gets an Attribute's value.
getAttrVal :: Attribute -> String
getAttrVal ((_,b)) = show b

-- | Converts an Attribute into a more parsable form.
convertAttributeToTuple :: Attribute -> (String, String)
convertAttributeToTuple at = (getAttrName at, getAttrVal at)

-- | Gets the children of the current node.
getChildren :: Content i -> [Content i]
getChildren = path [children]

-- | Gets the value of the attribute with the corresponding key.
getAttribute :: String -> Content i -> String
getAttribute attr (CElem content undefined)
    | null matchingAttrs = ""
    | otherwise = getAttrVal $ head matchingAttrs
    where matchingAttrs = filter (\x -> getAttrName x == attr)
                                 (getAttrs content)
getAttribute _ _ = ""

-- | Parses a transform String into a tuple of Float.
parseTransform :: String -> (Double, Double)
parseTransform transform =
    let parsedTransform = splitOn "," $ drop 10 transform
        xPos = read $ parsedTransform !! 0 :: Double
        yPos = read $ init $ parsedTransform !! 1 :: Double
    in (xPos, yPos)

-- | Parses a path's `d` attribute.
parsePathD :: String -> [(Double, Double)]
parsePathD d
    | head d == 'm' = foldCoordsRel coordList
    | otherwise =  processAbsCoords coordList
    where
      lengthMoreThanOne = \x -> length x > 1
      coordList = filter lengthMoreThanOne $ map (splitOn ",") $ splitOn " " d

-- | Converts a relative coordinate structure into an absolute one.
foldCoordsRel :: [[String]] -> [(Double, Double)]
foldCoordsRel dCoords =
    tail $
    foldl (\x y -> x ++ [addTuples (convertToFloatTuple y) (last x)])
          [(0,0)]
          dCoords

-- | Converts a relative coordinate structure into an absolute one.
processAbsCoords :: [[String]] -> [(Double, Double)]
processAbsCoords = map convertToFloatTuple

-- | Converts a list of String of length 2 into a tuple of Float.
convertToFloatTuple :: [String] -> (Double, Double)
convertToFloatTuple y = (read (head y) :: Double, read (last y) :: Double)

-- | Adds two tuples together.
addTuples :: (Double, Double) -> (Double, Double) -> (Double, Double)
addTuples (a,b) (c,d) = (a + c, b + d)

-- | Determines if a point intersects with a shape.
intersects :: Double -> Double -> (Double, Double) -> Double -> (Double, Double) -> Bool
intersects width height (rx, ry) offset (px, py) =
    let dx = px - rx
        dy = py - ry
        rationalOffset = offset
    in dx >= -1 * rationalOffset && dx <= width + rationalOffset && dy >= -1 * rationalOffset && dy <= height + rationalOffset;

-- | Removes the part of a string after the first forward slash.
dropSlash :: String -> String
dropSlash str = takeWhile (/='/') str