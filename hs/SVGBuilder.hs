{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}
module SVGBuilder where

import SVGTypes
import Tables
import Control.Monad.IO.Class  (liftIO)
import qualified Data.Conduit.List as CL
import Database.Persist
import Database.Persist.Sqlite
import Data.Conduit
import Data.List
import JsonParser

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

queryRects :: IO ()
queryRects = 
    runSqlite dbStr $ do
        sqlRects :: [Entity Rects] <- selectList [] []
        sqlTexts :: [Entity Texts] <- selectList [] []
        sqlPaths :: [Entity Paths] <- selectList [] []
        let rectXml = map (convertRectToXML . buildRect . entityVal) sqlRects
        let textXml = map (convertTextToXML . buildText . entityVal) sqlTexts
        let pathXml = map (convertPathToXML . buildPath . entityVal) sqlPaths
        liftIO $ writeFile "Testfile.svg" svgHeader
        liftIO $ appendFile "Testfile.svg" $ unwords pathXml
        liftIO $ appendFile "Testfile.svg" $ unwords rectXml
        liftIO $ appendFile "Testfile.svg" $ unwords textXml
        liftIO $ appendFile "Testfile.svg" svgFooter

printDB :: IO ()
printDB = runSqlite dbStr $ do
              let sql = "SELECT * FROM rects"
              rawQuery sql [] $$ CL.mapM_ (liftIO . print)

convertRectToXML :: Rect -> String
convertRectToXML rect = 
    "<rect x=\"" ++ 
    show (fromRational $ xPos rect) ++
    "\" y=\"" ++
    show (fromRational $ yPos rect) ++
    "\" width=\"" ++
    show (fromRational $ width rect) ++
    "\" height=\"" ++
    show (fromRational $ height rect) ++
    "\" style=\"" ++
    svgStyle ++
    "\"/>"

convertTextToXML :: Text -> String
convertTextToXML text = 
    "<text xml:space=\"preserve\" x=\"" ++ 
    (show $ fromRational $ textXPos text) ++
    "\" y=\"" ++
    (show $ fromRational $ textYPos text) ++
    "\" style=\"" ++
    (textStyle text) ++
    "\">" ++ (textText text) ++"</text>"

convertPathToXML :: Path -> String
convertPathToXML path = 
    "<path style=\"stroke:#000000;fill:none;\" d=\"M " ++ (buildPathString $ points path) ++ "\"/>"
         
buildRect :: Rects -> Rect
buildRect entity = 
    Rect (rectsWidth entity)
         (rectsHeight entity)
         (rectsXPos entity)
         (rectsYPos entity)
         (rectsStyle entity)

buildText :: Texts -> Text
buildText entity = 
    Text (textsXPos entity)
         (textsYPos entity)
         (textsText entity)
         (textsStyle entity)

buildPath :: Paths -> Path
buildPath entity = 
    Path (map point $ pathsD entity)
         (pathsStyle entity)

buildPathString :: [(Rational, Rational)] -> String
buildPathString d = intercalate " " $ map joinPathTuple $ map convertRationalTupToString d

joinPathTuple :: (String, String) -> String
joinPathTuple tup = (fst tup) ++ "," ++ (snd tup)

convertRationalTupToString :: (Rational, Rational) -> (String, String)
convertRationalTupToString tup = (show $ fromRational (fst tup), show $ fromRational (snd tup))
