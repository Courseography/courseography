{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}
module Main where

import Text.XML.HaXml
import Text.XML.HaXml.ByteStringPP
import Text.XML.HaXml.Wrappers
import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators
import Control.Monad.IO.Class  (liftIO, MonadIO)
import Text.XML.HaXml.Util
import Text.XML.HaXml.XmlContent.Parser
import qualified Data.Conduit.List as CL
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad
import Control.Monad.Trans.Reader
import Text.XML.HaXml.Namespaces
import Data.Conduit
import Data.List.Split
import Data.List
import Data.Text as T (pack, unpack)
import Database.Tables
import Database.JsonParser
import SvgParsing.SVGGenerator
import SvgParsing.SVGBuilder
import SvgParsing.SVGTypes
import SvgParsing.ParserUtil

main :: IO ()
main = do graphFile <- readFile "../res/graphs/graph_regions.svg"
          let graphDoc = xmlParse "output.error" graphFile
          print "Parsing SVG file..."
          runSqlite dbStr $ do
              runMigration migrateAll
              parseLevel False (Style (0,0) "" "") (getRoot graphDoc)
              liftIO $ print "Parsing complete"
          buildSVG
          liftIO $ print "SVG Built"

-- | Parses a level.
parseLevel :: MonadIO m0 =>  Bool -> Style -> Content i -> ReaderT SqlBackend m0 ()
parseLevel currentlyInRegion style content =
    if getAttribute "id" content == "layer2" ||
       (getName content == "defs")
      then return ()
      else do
           let isRegion       = getAttribute "id" content == "layer3"
               rects          = (tag "rect") content
               texts          = (tag "text") content
               paths          = (tag "path") content
               ellipses       = (tag "ellipse") content
               children       = getChildren content
               newTransform   = getAttribute "transform" content
               newStyle       = getAttribute "style" content
               newFill        = getNewStyleAttr newStyle "fill" (fill style)
               newStroke      = getNewStyleAttr newStyle "stroke" (stroke style)
               x = if null newTransform then (0,0) else parseTransform newTransform
               adjustedTransform = addTuples (transform style) x
               parentStyle = Style adjustedTransform
                                   newFill  
                                   newStroke

           parseElements (parseRect parentStyle) rects
           parseElements (parseText parentStyle) texts
           parseElements (parsePath (currentlyInRegion || isRegion) parentStyle) paths
           parseElements (parseEllipse parentStyle) ellipses
           parseChildren (currentlyInRegion || isRegion) parentStyle children

-- | Parses a list of Content.
parseChildren :: MonadIO m0 => Bool -> Style -> [Content i] -> ReaderT SqlBackend m0 ()
parseChildren _ _ [] = return ()
parseChildren currentlyInRegion style (x:xs) =
    do parseLevel currentlyInRegion style x
       parseChildren currentlyInRegion style xs

-- | Applies a parser to a list of Content.
parseElements :: MonadIO m0 => (Content i ->  ReaderT SqlBackend m0 ()) -> [Content i] -> ReaderT SqlBackend m0 ()
parseElements _ [] = return ()
parseElements f (x:xs) = do f x
                            parseElements f xs

-- | Parses a rect.
parseRect :: MonadIO m0 => Style -> Content i -> ReaderT SqlBackend m0 ()
parseRect style content = 
    insertRectIntoDB (getAttribute "id" content)
                     (read $ getAttribute "width" content :: Float)
                     (read $ getAttribute "height" content :: Float)
                     ((read $ getAttribute "x" content :: Float) + fst (transform style))
                     ((read $ getAttribute "y" content :: Float) + snd (transform style))
                     style

-- | Parses a path.
parsePath :: MonadIO m0 => Bool -> Style -> Content i -> ReaderT SqlBackend m0 ()
parsePath isRegion style content =
    unless (last (getAttribute "d" content) == 'z' && not isRegion) $
        insertPathIntoDB (map (addTuples (transform style)) $ parsePathD $ getAttribute "d" content)
                         style
                         isRegion

-- | Parses a text.
parseText :: MonadIO m0 => Style -> Content i -> ReaderT SqlBackend m0 ()
parseText style content = 
    insertTextIntoDB (getAttribute "id" content)
                     ((read $ getAttribute "x" content :: Float) + fst (transform style))
                     ((read $ getAttribute "y" content :: Float) + snd (transform style))
                     (tagTextContent content)
                     style

-- | Parses a text.
parseEllipse :: MonadIO m0 => Style -> Content i -> ReaderT SqlBackend m0 ()
parseEllipse style content = 
    insertEllipseIntoDB ((read $ getAttribute "cx" content :: Float) + fst (transform style))
                        ((read $ getAttribute "cy" content :: Float) + snd (transform style))
                        (read $ getAttribute "rx" content :: Float)
                        (read $ getAttribute "ry" content :: Float)
                        (fill style)

-- | Inserts an ellipse entry into the rects table.
insertEllipseIntoDB :: MonadIO m0 => Float -> Float -> Float -> Float -> String -> ReaderT SqlBackend m0 ()
insertEllipseIntoDB xPos yPos rx ry stroke =
        insert_ $ Ellipses (toRational xPos)
                           (toRational yPos)
                           (toRational rx)
                           (toRational ry)
                           stroke

-- | Inserts a rect entry into the rects table.
insertRectIntoDB :: MonadIO m0 => String -> Float -> Float -> Float -> Float -> Style -> ReaderT SqlBackend m0 ()
insertRectIntoDB id_ width height xPos yPos style =
        insert_ $ Rects 1
                        id_
                        (toRational width)
                        (toRational height)
                        (toRational xPos)
                        (toRational yPos)
                        (fill style)
                        (stroke style)
                        (fill style == "#a14c3a")

-- | Inserts a text entry into the texts table.
insertTextIntoDB :: MonadIO m0 => String -> Float -> Float -> String -> Style -> ReaderT SqlBackend m0 ()
insertTextIntoDB id_ xPos yPos text style =
        insert_ $ Texts 1
                        id_
                        (toRational xPos)
                        (toRational yPos)
                        text

-- | Inserts a tex entry into the texts table.
insertPathIntoDB :: MonadIO m0 => [(Float, Float)] -> Style -> Bool -> ReaderT SqlBackend m0 ()
insertPathIntoDB d style isRegion =
        insert_ $ Paths (map (Point . convertFloatTupToRationalTup) d)
                        (fill style)
                        (stroke style)
                        isRegion