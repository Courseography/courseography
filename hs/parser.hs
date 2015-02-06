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
import Data.Text as T (pack, unpack)
import Tables
import JsonParser

main :: IO ()
main = do x <- readFile "../res/graphs/graph_regions.svg"
          let y = xmlParse "output.error" x
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
          --parseLevel (0,0) $ getRoot y
          queryRects
          --printDB

svgHeader :: String
svgHeader = "<svg" ++
   " xmlns:dc=\"http://purl.org/dc/elements/1.1/\"" ++
   " xmlns:cc=\"http://creativecommons.org/ns#\"" ++
   " xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"" ++
   " xmlns:svg=\"http://www.w3.org/2000/svg\"" ++
   " xmlns=\"http://www.w3.org/2000/svg\"" ++
   " xmlns:xlink=\"http://www.w3.org/1999/xlink\"" ++
   " xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"" ++
   " xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"" ++
   " width=\"1052.3622\"" ++
   " height=\"744.09448\"" ++
   " id=\"svg2\"" ++
   " version=\"1.1\"" ++
   " sodipodi:docname=\"graph_regions.svg\"><g>"

svgFooter :: String
svgFooter = "</g></svg>"

svgStyle :: String
svgStyle = "fill:#8ccdf6;stroke:#8ccdf6"

printDB :: IO ()
printDB = runSqlite dbStr $ do
          let sql = "SELECT * FROM rects"
          rawQuery sql [] $$ CL.mapM_ (liftIO . print)

queryRects :: IO ()
queryRects = 
    runSqlite dbStr $ do
        sqlRects :: [Entity Rects] <- selectList [] []
        sqlTexts :: [Entity Texts] <- selectList [] []
        let rectXml = map (convertRectToXML . buildRect . entityVal) sqlRects
        let textXml = map (convertTextToXML . buildText . entityVal) sqlTexts
        liftIO $ writeFile "Testfile.svg" svgHeader
        liftIO $ appendFile "Testfile.svg" $ unwords rectXml
        liftIO $ appendFile "Testfile.svg" $ unwords textXml
        liftIO $ appendFile "Testfile.svg" svgFooter

convertRectToXML :: Rect -> String
convertRectToXML rect = "<rect x=\"" ++ 
                        (show $ fromRational $ xPos rect) ++
                        "\" y=\"" ++
                        (show $ fromRational $ yPos rect) ++
                        "\" width=\"" ++
                        (show $ fromRational $ width rect) ++
                        "\" height=\"" ++
                        (show $ fromRational $ height rect) ++
                        "\" style=\"" ++
                        svgStyle ++
                        "\"/>"

convertTextToXML :: Text -> String
convertTextToXML text = "<text xml:space=\"preserve\" x=\"" ++ 
                        (show $ fromRational $ textXPos text) ++
                        "\" y=\"" ++
                        (show $ fromRational $ textYPos text) ++
                        "\">" ++ (textText text) ++"</text>"

buildRect :: Rects -> Rect
buildRect entity = Rect (rectsWidth entity)
                        (rectsHeight entity)
                        (rectsXPos entity)
                        (rectsYPos entity)
                        (rectsStyle entity)

buildText :: Texts -> Text
buildText entity = Text (textsXPos entity)
                        (textsYPos entity)
                        (textsText entity)

parseLevel :: (Float, Float) -> Content i -> IO ()
parseLevel parentTransform content3 = do
  let rects = parseContent (tag "rect") content3
  let texts = parseContent (tag "text") content3
  let children = getChildren content3
  let transform = getAttribute "transform" content3
  let x = if null transform then (0,0) else parseTransform transform
  let adjustedTransform = (fst parentTransform + fst x, snd parentTransform + snd x)
  parseElements (parseRect adjustedTransform) rects
  parseElements (parseText adjustedTransform) texts
  parseChildren adjustedTransform children


parseChildren :: (Float, Float) -> [Content i] -> IO ()
parseChildren adjustedTransform [] = print "Level parsed"
parseChildren adjustedTransform (x:xs) = do parseLevel adjustedTransform x
                                            parseChildren adjustedTransform xs

--parseLevel :: Content i -> (Float, Float)--[Content i]
--parseLevel content3 = do
--  -- Get Rects
--  let rects = parseContent (tag "rect") content3
--  -- Get Children
--  let children = getChildren content3
--  --let x = foldl (++) [] (map parseLevel children)
--  let transform = getAttribute "transform" content3
--  let x = if null transform then (0,0) else parseTransform transform
--  let childrenTransformX = foldl (+) 0 (map fst $ map parseLevel $ getChildren content3)
--  let childrenTransformY = foldl (+) 0 (map snd $ map parseLevel $ getChildren content3)
--  (fst x + childrenTransformX, snd x + childrenTransformY)

parseElements :: (Content i -> IO ()) -> [Content i] -> IO ()
parseElements f (x:xs) = do f x
                            parseElements f xs
parseElements f [] = print "Done Rects"

parseRect :: (Float, Float) -> Content i -> IO ()
parseRect transform content = 
    insertRectIntoDB (getAttribute "id" content)
                     (read $ getAttribute "width" content :: Float)
                     (read $ getAttribute "height" content :: Float)
                     ((read $ getAttribute "x" content :: Float) + fst transform)
                     ((read $ getAttribute "y" content :: Float) + snd transform)
                     (getAttribute "style" content)

parseText :: (Float, Float) -> Content i -> IO ()
parseText transform content = insertTextIntoDB (getAttribute "id" content)
                                               ((read $ getAttribute "x" content :: Float) + fst transform)
                                               ((read $ getAttribute "y" content :: Float) + snd transform)
                                               (tagTextContent content) 

getRoot :: Document i -> Content i
getRoot doc = head $ parseDocument (tag "svg") doc

insertRectIntoDB :: String -> Float -> Float -> Float -> Float -> String -> IO ()
insertRectIntoDB id_ width height xPos yPos style = runSqlite dbStr $ do
        runMigration migrateAll
        insert_ $ Rects 1
                        id_
                        (toRational width)
                        (toRational height)
                        (toRational xPos)
                        (toRational yPos)
                        style

insertTextIntoDB :: String -> Float -> Float -> String -> IO ()
insertTextIntoDB id_ xPos yPos text = runSqlite dbStr $ do
        runMigration migrateAll
        insert_ $ Texts 1
                        id_
                        (toRational xPos)
                        (toRational yPos)
                        text

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

parseTransform :: String -> (Float, Float)
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
          } deriving Show

data Rect =
    Rect {
           width :: Rational,
           height :: Rational,
           xPos :: Rational,
           yPos :: Rational,
           style :: String
         } deriving Show

data Text =
    Text {
           textXPos :: Rational,
           textYPos :: Rational,
           textText :: String
         } deriving Show


data Path =
    Path {}