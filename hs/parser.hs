{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}
module Main where

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
import SVGBuilder
import SVGTypes

main :: IO ()
main = do graphFile <- readFile "../res/graphs/graph_regions.svg"
          let graphDoc = xmlParse "output.error" graphFile
          parseLevel (0,0) "" $ getRoot graphDoc
          buildSVG
          printDB

-- | Parses a level.
parseLevel :: (Float, Float) -> String -> Content i -> IO ()
parseLevel parentTransform parentFill content = do
    if (getAttribute "id" content) == "layer3" || 
       (getAttribute "id" content) == "layer2"
      then liftIO $ print "Abort"
      else do
           let rects = parseContent (tag "rect") content
           let texts = parseContent (tag "text") content
           let paths = parseContent (tag "path") content
           let children = getChildren content
           let transform = getAttribute "transform" content
           let style = getAttribute "style" content
           let fill = getFill style
           let fillx = if null fill then parentFill else fill
           let filly = if fillx == "none" then parentFill else fillx
           let fillz = if fillx == "#000000" then "none" else filly
           let x = if null transform then (0,0) else parseTransform transform
           let adjustedTransform = (fst parentTransform + fst x,
                                    snd parentTransform + snd x)
           parseElements (parseRect adjustedTransform fillz) rects
           parseElements (parseText adjustedTransform style) texts
           parseElements (parsePath adjustedTransform) paths
           parseChildren adjustedTransform fillz children

-- | Parses a list of Content.
parseChildren :: (Float, Float) -> String -> [Content i] -> IO ()
parseChildren adjustedTransform parentFill [] = print "Level parsed..."
parseChildren adjustedTransform parentFill (x:xs) = do parseLevel adjustedTransform parentFill x
                                                       parseChildren adjustedTransform parentFill xs

-- | Gets the fill from a style String.
getFill :: String -> String
getFill style = drop 5 $ head $ (filter (\x -> take 4 x == "fill") $ splitOn ";" style) ++ [""]

-- | Applies a parser to a list of Content.
parseElements :: (Content i -> IO ()) -> [Content i] -> IO ()
parseElements f [] = print "Finished elements..."
parseElements f (x:xs) = do f x
                            parseElements f xs

-- | Parses a rect.
parseRect :: (Float, Float) -> String -> Content i -> IO ()
parseRect transform parentFill content = 
    insertRectIntoDB (getAttribute "id" content)
                     (read $ getAttribute "width" content :: Float)
                     (read $ getAttribute "height" content :: Float)
                     ((read $ getAttribute "x" content :: Float) + fst transform)
                     ((read $ getAttribute "y" content :: Float) + snd transform)
                     ("fill:" ++ parentFill)

-- | Parses a path.
parsePath :: (Float, Float) -> Content i -> IO ()
parsePath transform content = 
    insertPathIntoDB (map (addTransform transform) $ parsePathD $ getAttribute "d" content)
                     (getAttribute "style" content)

-- | Parses a text.
parseText :: (Float, Float) -> String -> Content i -> IO ()
parseText transform parentStyle content = insertTextIntoDB (getAttribute "id" content)
                                               ((read $ getAttribute "x" content :: Float) + fst transform)
                                               ((read $ getAttribute "y" content :: Float) + snd transform)
                                               (tagTextContent content) 
                                               parentStyle

-- | Gets the root element of the document.
getRoot :: Document i -> Content i
getRoot doc = head $ parseDocument (tag "svg") doc

-- | Inserts a rect entry into the rects table.
insertRectIntoDB :: String -> Float -> Float -> Float -> Float -> String -> IO ()
insertRectIntoDB id_ width height xPos yPos style = 
    runSqlite dbStr $ do
        runMigration migrateAll
        insert_ $ Rects 1
                        id_
                        (toRational width)
                        (toRational height)
                        (toRational xPos)
                        (toRational yPos)
                        style

-- | Inserts a text entry into the texts table.
insertTextIntoDB :: String -> Float -> Float -> String -> String -> IO ()
insertTextIntoDB id_ xPos yPos text style = 
    runSqlite dbStr $ do
        runMigration migrateAll
        insert_ $ Texts 1
                        id_
                        (toRational xPos)
                        (toRational yPos)
                        text
                        style

-- | Inserts a tex entry into the texts table.
insertPathIntoDB :: [(Float, Float)] -> String -> IO ()
insertPathIntoDB d style = 
    runSqlite dbStr $ do
        runMigration migrateAll
        insert_ $ Paths (map (Point . convertFloatTupToRationalTup) d)
                        style

-- | Adds one tuple to the second tuple.
-- NOTE: Can be replaced by addTuples.
addTransform :: (Float, Float) -> (Float, Float) -> (Float, Float)
addTransform tup transTup = (fst tup + fst transTup, snd tup + snd transTup)

-- | Converts a tuple of Float to a tuple of Rational.
convertFloatTupToRationalTup :: (Float, Float) -> (Rational, Rational)
convertFloatTupToRationalTup tup = (toRational (fst tup), toRational (snd tup))

--filterAttrVal :: [Attribute] -> String -> String
--filterAttrVal attrs attrName = snd $ head $ filter (\x -> snd x == attrName) $ map convertAttributeToTuple attrs

-- | Applys a CFilter to a Document and produces a list of Content filtered 
-- by the CFilter.
parseDocument :: CFilter i -> Document i -> [Content i]
parseDocument filter (Document p s e m) = filter (CElem e undefined)

-- | Applys a CFilter to a Content and produces a list of Content filtered by the
-- CFilter.
parseContent :: CFilter i -> Content i -> [Content i]
parseContent filter content = filter content

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

-- | Gets the children of the current node.
getChildren :: Content i -> [Content i]
getChildren content = parseContent (path [children]) content

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
parseTransform transform = do
    let commaPos = getComma 0 $ drop 9 transform
    let xPos = read $ drop 1 $ take commaPos $ drop 9 transform :: Float
    let yPos = read $ init $ drop (commaPos + 1) $ drop 9 transform :: Float
    (xPos, yPos)

-- | Gets the location of a comma in a string.
-- NOTE: Can probably be replaced by Data.List.Split.
getComma :: Int -> String -> Int
getComma accum x = if head x == ',' then accum else getComma (accum + 1) (tail x)

-- | Parses a path's `d` attribute.
parsePathD :: String -> [(Float, Float)]--[(Rational, Rational)]
parsePathD d = tail (if head d == 'm'
                     then foldCoords $ filter (\x -> length x > 1) $ map (splitOn ",") $ splitOn " " d
                     else foldCoords $ filter (\x -> length x > 1) $ map (splitOn ",") $ splitOn " " d)

-- | Converts a relative coordinate structure into an absolute one.
foldCoords :: [[String]] -> [(Float, Float)]
foldCoords dCoords = foldl (\x y -> x ++ [(addTuples (convertToFloatTuple y) (last x))]) [(0,0)] dCoords

-- | Converts a list of String of length 2 into a tuple of Float.
convertToFloatTuple :: [String] -> (Float, Float)
convertToFloatTuple y = (read (head y) :: Float, read (last y) :: Float)

-- | Adds two tuples together.
addTuples :: (Float, Float) -> (Float, Float) -> (Float, Float)
addTuples tup1 tup2 = (fst tup1 + fst tup2, snd tup1 + snd tup2)