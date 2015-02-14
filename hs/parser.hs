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
          parseLevel (Style (0,0) "" "none" "none" "none" "none" "none") (getRoot graphDoc)
          buildSVG
          printDB

-- | Parses a level.
parseLevel :: Style -> Content i -> IO ()
parseLevel style content = do
    if (getAttribute "id" content) == "layer2" ||
       ((getName content) == "defs")
      then liftIO $ print "Abort"
      else do
           let rects = parseContent (tag "rect") content
           let texts = parseContent (tag "text") content
           let paths = parseContent (tag "path") content
           let children = getChildren content
           let newTransform = getAttribute "transform" content
           let newStyle = getAttribute "style" content
           let newFill = getStyleAttr "fill" (fontSize style)
           let newFontSize = getNewStyleAttr newStyle "font-size" (fontSize style)
           let newStroke = getNewStyleAttr newStyle "stroke" (stroke style)
           let newFillOpacity = getNewStyleAttr newStyle "fill-opacity" (fillOpacity style)
           let newFontWeight = getNewStyleAttr newStyle "font-weight" (fontWeight style)
           let newFontFamily = getNewStyleAttr newStyle "font-family" (fontFamily style)
           let x = if null newTransform then (0,0) else parseTransform newTransform
           let adjustedTransform = (fst (transform style) + fst x,
                                    snd (transform style) + snd x)
           let parentStyle = Style adjustedTransform 
                                   newFill  
                                   newFontSize  
                                   newStroke 
                                   newFillOpacity 
                                   newFontWeight
                                   newFontFamily
           parseElements (parseRect parentStyle) rects
           parseElements (parseText parentStyle) texts
           parseElements (parsePath parentStyle) paths
           parseChildren parentStyle children


getNewStyleAttr :: String -> String -> String -> String
getNewStyleAttr newStyle attr parent = if (null (getStyleAttr attr newStyle) ||
                                           (getStyleAttr attr newStyle) == "none") 
                                       then parent
                                       else (getStyleAttr attr newStyle)

-- | Parses a list of Content.
parseChildren :: Style -> [Content i] -> IO ()
parseChildren _ [] = return ()
parseChildren style (x:xs) =
    do parseLevel style x
       parseChildren style xs

-- | Gets the fill from a style String.
getStyleAttr :: String -> String -> String
getStyleAttr attr style = drop ((length attr) + 1) $ head $ (filter (\x -> take ((length attr) + 1) x == (attr ++ ":")) $ splitOn ";" style) ++ [""]

-- | Applies a parser to a list of Content.
parseElements :: (Content i -> IO ()) -> [Content i] -> IO ()
parseElements f [] = return ()
parseElements f (x:xs) = do f x
                            parseElements f xs

-- | Parses a rect.
parseRect :: Style -> Content i -> IO ()
parseRect style content = 
    insertRectIntoDB (getAttribute "id" content)
                     (read $ getAttribute "width" content :: Float)
                     (read $ getAttribute "height" content :: Float)
                     ((read $ getAttribute "x" content :: Float) + fst (transform style))
                     ((read $ getAttribute "y" content :: Float) + snd (transform style))
                     style

-- | Parses a path.
parsePath :: Style -> Content i -> IO ()
parsePath style content = 
    insertPathIntoDB (map (addTransform (transform style)) $ parsePathD $ getAttribute "d" content)
                     style
                     --(getAttribute "clip-path" content)

-- | Parses a text.
parseText :: Style -> Content i -> IO ()
parseText style content = 
    insertTextIntoDB (getAttribute "id" content)
                     ((read $ getAttribute "x" content :: Float) + fst (transform style))
                     ((read $ getAttribute "y" content :: Float) + snd (transform style))
                     (tagTextContent content)
                     style

-- | Gets the root element of the document.
getRoot :: Document i -> Content i
getRoot doc = head $ parseDocument (tag "svg") doc

-- | Inserts a rect entry into the rects table.
insertRectIntoDB :: String -> Float -> Float -> Float -> Float -> Style -> IO ()
insertRectIntoDB id_ width height xPos yPos style = 
    runSqlite dbStr $ do
        runMigration migrateAll
        insert_ $ Rects 1
                        id_
                        (toRational width)
                        (toRational height)
                        (toRational xPos)
                        (toRational yPos)
                        (fill style)
                        (stroke style)
                        (fillOpacity style)

-- | Inserts a text entry into the texts table.
insertTextIntoDB :: String -> Float -> Float -> String -> Style -> IO ()
insertTextIntoDB id_ xPos yPos text style = 
    runSqlite dbStr $ do
        runMigration migrateAll
        insert_ $ Texts 1
                        id_
                        (toRational xPos)
                        (toRational yPos)
                        text
                        (fontSize style)
                        (fontWeight style)
                        (fontFamily style)

-- | Inserts a tex entry into the texts table.
insertPathIntoDB :: [(Float, Float)] -> Style -> IO ()
insertPathIntoDB d style = 
    runSqlite dbStr $ do
        runMigration migrateAll
        insert_ $ Paths (map (Point . convertFloatTupToRationalTup) d)
                        (fill style)
                        (fillOpacity style)
                        (stroke style)
                        --clipPath

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
parsePathD d = (if head d == 'm'
                     then foldCoordsRel $ filter (\x -> length x > 1) $ map (splitOn ",") $ splitOn " " d
                     else processAbsCoords $ filter (\x -> length x > 1) $ map (splitOn ",") $ splitOn " " d)

-- | Converts a relative coordinate structure into an absolute one.
foldCoordsRel :: [[String]] -> [(Float, Float)]
foldCoordsRel dCoords = tail $ foldl (\x y -> x ++ [(addTuples (convertToFloatTuple y) (last x))]) [(0,0)] dCoords

-- | Converts a relative coordinate structure into an absolute one.
processAbsCoords :: [[String]] -> [(Float, Float)]
processAbsCoords dCoords = map convertToFloatTuple dCoords


-- | Converts a list of String of length 2 into a tuple of Float.
convertToFloatTuple :: [String] -> (Float, Float)
convertToFloatTuple y = (read (head y) :: Float, read (last y) :: Float)

-- | Adds two tuples together.
addTuples :: (Float, Float) -> (Float, Float) -> (Float, Float)
addTuples tup1 tup2 = (fst tup1 + fst tup2, snd tup1 + snd tup2)