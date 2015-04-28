-- |Provide some utility functions for the SVG parsing.
module Svg.ParserUtil where

import Text.XML.HaXml hiding (find)
import Text.XML.HaXml.Namespaces (printableName)
import Data.List.Split (splitOn)
import Data.List
import Data.Maybe (fromMaybe)
import Data.Text as T (pack, unpack)
import Database.Tables (Point)

-- | Gets the root element of the document.
getRoot :: Document i -> Content i
getRoot doc = head $ parseDocument (tag "svg") doc

-- | Gets a style attribute from a style String.
getStyleAttr :: String -> String -> String
getStyleAttr attr style =
    drop (length attr + 1) $
    fromMaybe "" (find (isPrefixOf $ attr ++ ":") (splitOn ";" style))

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
getAttrName (a, _) = printableName a

-- | Gets an Attribute's value.
getAttrVal :: Maybe Attribute -> String
getAttrVal Nothing = ""
getAttrVal (Just (_, b)) = show b

-- | Converts an Attribute into a more parsable form.
convertAttributeToTuple :: Attribute -> (String, String)
convertAttributeToTuple at = (getAttrName at, getAttrVal (Just at))

-- | Gets the value of the attribute with the corresponding key.
getAttribute :: String -> Content i -> String
getAttribute attr (CElem content undefined) =
    let matchingAttrs = find (\x -> getAttrName x == attr)
                             (getAttrs content)
    in getAttrVal matchingAttrs
getAttribute _ _ = ""

-- | Parses a transform String into a tuple of Float.
parseTransform :: String -> Point
parseTransform "" = (0,0)
parseTransform transform =
    let parsedTransform = splitOn "," $ drop 10 transform
        xPos = read $ parsedTransform !! 0
        yPos = read $ init $ parsedTransform !! 1
    in (xPos, yPos)

-- | Parses a path's `d` attribute.
parsePathD :: String -- ^ The 'd' attribute of an SVG path.
           -> [Point]
parsePathD d
    | head d == 'm' = relCoords
    | otherwise = absCoords
    where
      lengthMoreThanOne x = length x > 1
      coordList = filter lengthMoreThanOne (map (splitOn ",") $ splitOn " " d)
      -- Converts a relative coordinate structure into an absolute one.
      relCoords = tail $ foldl (\x y -> x ++ [addTuples (convertToPoint y)
                                                        (last x)])
                               [(0,0)]
                               coordList
      -- Converts a relative coordinate structure into an absolute one.
      absCoords = map convertToPoint coordList
      convertToPoint y = (read (head y), read (last y))

-- | Adds two tuples together.
addTuples :: Point -> Point -> Point
addTuples (a,b) (c,d) = (a + c, b + d)

-- | Determines if a point intersects with a shape.
intersects :: Double -- ^ The shape's width.
           -> Double -- ^ The shape's height.
           -> Point  -- ^ The shape's coordinate.
           -> Double -- ^ The offset.
           -> Point  -- ^ The point's coordinate.
           -> Bool
intersects width height (rx, ry) offset (px, py) =
    let dx = px - rx
        dy = py - ry
    in dx >= -1 * offset &&
       dx <= width + offset &&
       dy >= -1 * offset &&
       dy <= height + offset;

-- | Reads an attribute of an element.
readAttr :: Read a => String    -- ^ The attribute's name.
                   -> Content i -- ^ The element that contains the attribute.
                   -> a
readAttr attr content = read $ getAttribute attr content
