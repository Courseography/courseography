{-|
Description: The main graph module. __Start here__.

This module is reponsible for parsing the SVG files exported from Inkscape.
It also currently acts as a main driver for the whole graph pipeline:

1. Parsing the raw SVG files
2. Inserting them into the database (see "Svg.Database")
3. Retrieving the database values and generating a new SVG file
   (See "Svg.Builder" and "Svg.Generator")

The final svg files are output in @public\/res\/graphs\/gen@ and are sent
directly to the client when viewing the @/graph@ page.
-}

module Svg.Parser
    (parsePrebuiltSvgs) where

import Data.Maybe (fromMaybe)
import qualified Text.HTML.TagSoup as TS hiding (fromAttrib)
import Database.Persist.Sqlite (runSqlite, SqlPersistM)
import Text.HTML.TagSoup (Tag)
import Control.Monad.IO.Class (liftIO)
import Database.Tables hiding (graphWidth, paths, texts, shapes, graphHeight)
import Database.DataType
import Svg.Database (insertGraph, insertElements, deleteGraphs)
import Config (graphPath, databasePath)
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)
import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Text.IO as T (readFile)
import Data.List.Split (splitOn)
import Data.Functor.Identity (Identity)

-- A Parsec parser with user defined state. This can be made stateless by
-- assigning () to u.
type StatefulParser u a = P.ParsecT String u Identity a

parsePrebuiltSvgs :: IO ()
parsePrebuiltSvgs = runSqlite databasePath $ do
    deleteGraphs
    performParse "Computer Science" "csc2019.svg"
    performParse "Statistics" "sta2017.svg"
    performParse "Biochemistry" "bch2015.svg"
    performParse "Cell & Systems Biology" "csb2015.svg"
    performParse "Estonian" "est2015.svg"
    performParse "Finnish" "fin2015.svg"
    performParse "Italian" "ita2015.svg"
    performParse "Linguistics" "lin2015.svg"
    performParse "Rotman" "rotman2015.svg"
    performParse "Economics" "eco2015.svg"
    performParse "Spanish" "spa2015.svg"
    performParse "Portuguese" "prt2015.svg"
    performParse "Slavic"  "sla2015.svg"
    performParse "East Asian Studies" "eas2015.svg"
    performParse "English" "eng2015.svg"
    performParse "History and Philosophy of Science" "hps2015.svg"
    performParse "History" "his2015.svg"
    performParse "Geography" "ggr2015.svg"
    performParse "Aboriginal" "abs2015.svg"
    performParse "German" "ger2015.svg"


-- | The starting point for parsing a graph with a given title and file.
performParse :: T.Text -- ^ The title of the graph.
             -> String -- ^ The filename of the file that will be parsed.
             -> SqlPersistM ()
performParse graphName inputFilename = do
    liftIO . print $ "Parsing graph " ++ T.unpack graphName ++ " from file " ++ inputFilename
    graphFile <- liftIO $ T.readFile (graphPath ++ inputFilename)
    let tags = TS.parseTags graphFile
        svgRoot = head $ filter (TS.isTagOpenName "svg") tags
        (graphWidth, graphHeight) = parseSize svgRoot
    key <- insertGraph graphName graphWidth graphHeight

    -- Need to remove any "defs" which Inkscape added
    let defs = TS.partitions (TS.isTagOpenName "defs") tags
        tagsWithoutDefs =
            if null defs
            then
                tags
            else
                -- TODO: pull this out into a generic helper
                takeWhile (not . TS.isTagOpenName "defs") tags ++
                concatMap (dropWhile (not . TS.isTagCloseName "defs")) defs

        parsedGraph = parseGraph key tagsWithoutDefs

    -- Insert the graph components into the database
    insertElements parsedGraph


-- * Parsing functions

-- | Parse the height and width dimensions from the SVG element, respectively,
-- and return them as a tuple.
parseSize :: Tag T.Text   -- ^ The file contents of the graph that will be parsed.
          -> (Double, Double)
parseSize svgRoot =
    (parseDouble "width" svgRoot, parseDouble "height" svgRoot)
    where parseDouble = parseAttr double


-- | Parses an SVG file.
--
-- This and the following functions traverse the raw SVG tags and return
-- three lists, each containing values corresponding to different graph elements
-- (edges, nodes, and text).
parseGraph ::  GraphId                 -- ^ The unique identifier of the graph.
           -> [Tag T.Text]             -- ^ The tags of the graph.
           -> ([Path],[Shape],[Text])
parseGraph key tags =
    let gTags = TS.partitions (TS.isTagOpenName "g") tags
        globalTransform = getTransform $ head $ head gTags
        ellipses = concatMap (parseEllipse key) gTags
        paths = concatMap (parsePath key) gTags
        rects = concatMap (parseRect key) gTags
        texts = concatMap (parseText key) gTags
        shapes = removeRedundant (ellipses ++ rects)

        paths' = map (\p -> p { pathPoints = map (addTuples globalTransform) $ pathPoints p}) paths
        shapes' = map (\s -> s { shapePos = addTuples globalTransform (shapePos s)}) $ filter small shapes
        texts' = map (\t -> t { textPos = addTuples globalTransform (textPos t)}) texts
    in
        (paths', shapes', texts')
    where
        -- Raw SVG seems to have a rectangle the size of the whole image
        small shape = shapeWidth shape < 300
        removeRedundant shapes =
            filter (not . \s -> shapePos s `elem` map shapePos shapes &&
                                (T.null (shapeFill s) || shapeFill s == "#000000") &&
                                elem (shapeType_ s) [Node, Hybrid]) shapes



-- | Create text values from g tags.
-- This searches for nested tspan tags inside text tags using a recursive
-- helper function.
parseText :: GraphId -> [Tag T.Text] -> [Text]
parseText key tags =
    let trans = getTransform $ head tags
        textTags = TS.partitions (TS.isTagOpenName "text") tags
        texts = concatMap (parseTextHelper key [] trans) textTags
    in
        texts


parseTextHelper :: GraphId -- ^ The Text's corresponding graph identifier.
                -> [(T.Text, T.Text)]
                -> Point
                -> [Tag T.Text]
                -> [Text]
parseTextHelper key styles' trans textTags =
    if not $ any (TS.isTagOpenName "tspan") (tail textTags)
    then
        [Text key
              (fromAttrib "id" $ head textTags) -- TODO: Why are we setting an id?
              (addTuples newTrans (readAttr "x" $ head textTags,
                                   readAttr "y" $ head textTags))
              (TS.escapeHTML $ trim $ TS.innerText textTags)
              align
              fill
        ]
    else
        let tspanTags = TS.partitions (TS.isTagOpenName "tspan") textTags
        in
            concatMap (parseTextHelper key newStyle newTrans) tspanTags
    where
        newStyle = styles (head textTags) ++ styles'
        currTrans = getTransform $ head textTags
        newTrans = addTuples trans currTrans
        alignAttr = styleVal "text-anchor" newStyle
        align = if T.null alignAttr
                then "begin"
                else alignAttr
        fill = styleVal "fill" newStyle


-- | Create a rectangle from a list of attributes.
parseRect :: GraphId -- ^ The Rect's corresponding graph identifier.
          -> [Tag T.Text]
          -> [Shape]
parseRect key tags =
    let
        rectOpenTags = filter (\tag -> TS.isTagOpenName "rect" tag || TS.isTagOpenName "polygon" tag) tags
    in
        map (\tag -> if (TS.isTagOpenName "rect" tag) then makeRect tag else makePoly tag) rectOpenTags
    where
        gOpen = head tags
        styles' = styles gOpen
        fill = styleVal "fill" styles'
        trans = getTransform $ head tags
        makeRect rectOpenTag =
            updateShape fill $
                Shape key
                  ""
                  (readAttr "x" rectOpenTag + fst trans,
                   readAttr "y" rectOpenTag + snd trans)
                  (readAttr "width" rectOpenTag)
                  (readAttr "height" rectOpenTag)
                  fill
                  ""
                  []
                  Node
        makePoly polyOpenTag =
          let points = map (\coord -> parseCoord $ T.pack coord) $ splitOn " " $ T.unpack $ fromAttrib "points" polyOpenTag
          in
            updateShape (fromAttrib "fill" polyOpenTag) $
              Shape key
                ""
                ((fst $ points !! 1) + fst trans, -- get x value
                (snd $ points !! 1) + snd trans) -- get y value
                ((fst $ points !! 0) - (fst $ points !! 1)) -- calculate width
                ((snd $ points !! 2) - (snd $ points !! 1)) -- calculate height
                fill
                ""
                []
                Node


-- | Create a path from a list of tags.
parsePath :: GraphId
          -> [Tag T.Text]
          -> [Path]
parsePath key tags =
    concatMap (parsePathHelper key trans) (filter (TS.isTagOpenName "path") tags)
    where
        trans = getTransform $ head tags


parsePathHelper :: GraphId -- ^ The Path's corresponding graph identifier.
                -> Point
                -> Tag T.Text
                -> [Path]
parsePathHelper key trans pathTag =
    let d = fromAttrib "d" pathTag
        styles' = styles pathTag
        currTrans = parseTransform $ fromAttrib "transform" pathTag
        realD = map (addTuples (addTuples trans currTrans)) $ parsePathD d
        fillAttr = styleVal "fill" styles'
        isRegion = not (T.null fillAttr) && fillAttr /= "none"
    in
        if T.null d || null realD || (T.last d == 'z' && not isRegion)
    then []
    else [updatePath fillAttr $
            Path key
                ""
                realD
                ""
                ""
                isRegion
                ""
                ""]


-- | Create an ellipse from an open ellipse tag.
parseEllipse :: GraphId
             -> [Tag T.Text]
             -> [Shape]
parseEllipse key tags =
    map (parseEllipseHelper key trans) (filter (TS.isTagOpenName "ellipse") tags)
    where
        trans = getTransform $ head tags


parseEllipseHelper :: GraphId     -- ^ The related graph id.
                   -> Point       -- ^ The translation to apply.
                   -> Tag T.Text  -- ^ The open ellipse tag.
                   -> Shape
parseEllipseHelper key (dx, dy) ellipseTag =
    Shape key
          ""
          (readAttr "cx" ellipseTag + dx,
           readAttr "cy" ellipseTag + dy)
          (readAttr "rx" ellipseTag * 2)
          (readAttr "ry" ellipseTag * 2)
          ""
          ""
          []
          BoolNode


-- * Helpers for parsing

-- | Looks up an attribute value and convert to another type.
-- Raises an exception if the attribute is not found.
readAttr :: Read a => T.Text    -- ^ The attribute's name.
                   -> Tag T.Text -- ^ The element that contains the attribute.
                   -> a
readAttr attr tag = fromMaybe
    (error ("reading " ++ T.unpack attr ++ " from " ++ show tag))
    (readMaybe $ T.unpack $ fromAttrib attr tag)

-- | Runs a parser on a text object.
-- Throws an exception on any parse errors.
parseValWithState :: StatefulParser u a -> u -> T.Text -> a
parseValWithState parser initialState input =
    case P.runParser parser initialState "" $ T.unpack input of
        Left err ->
            error ("reading " ++ T.unpack input ++ ":" ++ show err)
        Right val -> val

-- | Runs a parser on a text object without internal parser state.
-- Throws an exception on any parse errors.
parseVal :: Parser a -> T.Text -> a
parseVal parser input = parseValWithState parser () input

-- | Looks up an attribute value using the given parser.
parseAttr :: Parser a -> T.Text     -- ^ The attribute's name.
                      -> Tag T.Text -- ^ The element containing the attribute.
                      -> a
parseAttr parser attr tag = parseVal parser $ fromAttrib attr tag

-- | Parses one or more digit characters.
digits :: StatefulParser u String
digits = P.many1 P.digit

-- | Parses a double value, ignoring units if present.
double :: StatefulParser u Double
double = do
    sign <- P.option ' ' (P.char '-')
    whole <- digits
    fractional <- P.option ".0" parseFractional
    return (read $ sign : whole ++ fractional)
    where
        parseFractional = do
            _ <- P.char '.'
            decimals <- digits
            return ("." ++ decimals)

point :: StatefulParser u Point
point = do
    xPos <- double
    _ <- P.char ',' <|> P.space
    yPos <- double
    return (xPos, yPos)

-- | Return a list of styles from the style attribute of an element.
-- Every style has the form (name, value).
styles :: Tag T.Text -> [(T.Text, T.Text)]
styles tag =
    let styleStr = fromAttrib "style" tag
    in map toStyle $ T.splitOn ";" styleStr
    where
        toStyle split =
            case T.splitOn ":" split of
            [n,v] -> (n, v)
            _ -> ("","")


-- | Gets a style attribute from a style string.
styleVal :: T.Text -> [(T.Text, T.Text)] -> T.Text
styleVal nameStr styleMap = fromMaybe "" $ lookup nameStr styleMap


-- | Gets transform attribute from a tag, and parses it.
getTransform :: Tag T.Text -> Point
getTransform = parseTransform . fromAttrib "transform"


-- | Parses a translation String into a tuple of Float, ignoring scaling and
-- rotation.
parseTransform :: T.Text -> Point
parseTransform "" = (0, 0)
parseTransform transform =
    parseVal parser transform
    where
        parser = P.many (scale <|> rotate) >> translate
        scale = P.string "scale(" >> double >> P.spaces >> P.option 0 double
            >> P.char ')' >> P.spaces
        rotate = P.string "rotate(" >> double >> P.char ')' >> P.spaces
        translate = do
            _ <- P.string "translate("
            point

parseCoord :: T.Text -> Point
parseCoord "" = (0, 0)
parseCoord coord =
    parseVal point coord


-- | Parses a path's `d` attribute.
-- See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d>.
parsePathD :: T.Text -- ^ The 'd' attribute of an SVG path.
           -> [Point]
parsePathD d = parseValWithState parser initialState d
    where
        initialState :: (Point, Point)
        -- Keep track of the (first point, most recent point)
        initialState = ((0, 0), (0, 0))
        updateMostRecent p (first, _) = (first, p)
        parser = do
            p <- parseStep
            -- Record the first point added.
            _ <- P.modifyState $ \_ -> (p, p)
            rest <- P.many parseStep
            return (p : rest)
        parseStep = do
            _ <- (P.option ' ' $ bezier <|> absoluteMove) >> P.spaces
            p <- (closeLoop <|> relativeMove <|>
                absHorizontalMove <|> absVerticalMove <|>
                relHorizontalMove <|> relVerticalMove <|> point)
            -- Every time a new point is added, update the user state.
            _ <- P.spaces >> (P.modifyState $ updateMostRecent p)
            return p

        bezier = -- Discard the next two points. TODO: Add full support for Bezier curves
            (P.char 'C' <|> P.char 'c') >> P.spaces >> point >> P.spaces >> point >> return ' '
        closeLoop = do
            _ <- (P.char 'Z' <|> P.char 'z')
            (first, _) <- P.getState
            return first
        absoluteMove = (P.char 'L' <|> P.char 'M')
        relativeMove = do
            deltaP <- P.char 'm' >> P.spaces >> point
            (_, mostRecent) <- P.getState
            return (addTuples deltaP mostRecent)
        absHorizontalMove = do
            newX <- P.char 'H' >> P.spaces >> double
            (_, (_, currentY)) <- P.getState
            return (newX, currentY)
        absVerticalMove = do
            newY <- P.char 'V' >> P.spaces >> double
            (_, (currentX, _)) <- P.getState
            return (currentX, newY)
        relHorizontalMove = do
            deltaX <- P.char 'h' >> P.spaces >> double
            (_, (currentX, currentY)) <- P.getState
            return (currentX + deltaX, currentY)
        relVerticalMove = do
            deltaY <- P.char 'v' >> P.spaces >> double
            (_, (currentX, currentY)) <- P.getState
            return (currentX, currentY + deltaY)


-- * Other helpers

-- | These functions are used to update the parsed values
-- with fill inherited from their parents.
--
-- Eventually, it would be nice if we removed these functions and
-- simply passed everything down when making the recursive calls.

updatePath :: T.Text -- ^ The fill that may be added to the Path.
           -> Path
           -> Path
updatePath fill p =
    p { pathFill = if T.null (pathFill p) then fill else pathFill p }

updateShape :: T.Text -- ^ The fill that may be added to the Shape.
            -> Shape
            -> Shape
updateShape fill r =
    r { shapeFill = if T.null (shapeFill r) || shapeFill r == "none"
                    then fill
                    else shapeFill r,
        shapeType_ = if fill == "#888888" then Hybrid
                     else case shapeType_ r of
                              Hybrid   -> Hybrid
                              BoolNode -> BoolNode
                              Node     -> Node
                              _ -> undefined
      }

-- | Adds two tuples together.
addTuples :: Point -> Point -> Point
addTuples (a,b) (c,d) = (a + c, b + d)

-- | Helper to remove leading and trailing whitespace.
trim :: T.Text -> T.Text
trim = f . f
    where f = T.reverse . T.dropWhile isSpace

-- | Extract an attribute, crashes if not a 'TagOpen'.
--   Returns @\"\"@ if no attribute present.
fromAttrib :: T.Text -> Tag T.Text -> T.Text
fromAttrib att (TS.TagOpen _ atts) = fromMaybe T.empty $ lookup att atts
fromAttrib _ x = error $ show x ++ " is not a TagText"
