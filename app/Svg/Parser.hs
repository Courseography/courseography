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
    (parsePrebuiltSvgs, parseDynamicSvg, matrixPointMultiply) where

import Config (graphPath, runDb)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.List as List
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.IO as T (readFile)
import Database.DataType
import Database.Persist.Sqlite
import Database.Tables hiding (graphHeight, graphWidth, paths, shapes, texts)
import Svg.Database (deleteGraph, insertElements, insertGraph)
import qualified Text.HTML.TagSoup as TS hiding (fromAttrib)
import Text.HTML.TagSoup (Tag)
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)
import Util.Helpers


parsePrebuiltSvgs :: IO ()
parsePrebuiltSvgs = runDb $ do
    performParse "Computer Science" "csc2025.svg"
    performParse "Statistics" "sta2022.svg"
    -- performParse "(unofficial) Mathematics Specialist" "math_specialist2022.svg"
    -- performParse "(unofficial) Biochemistry" "bch2015.svg"
    -- performParse "(unofficial) Cell & Systems Biology" "csb2015.svg"
    -- performParse "(unofficial) Estonian" "est2015.svg"
    -- performParse "(unofficial) Finnish" "fin2015.svg"
    -- performParse "(unofficial) Italian" "ita2015.svg"
    -- performParse "(unofficial) Linguistics" "lin2015.svg"
    -- performParse "(unofficial) Rotman" "rotman2015.svg"
    -- performParse "(unofficial) Economics" "eco2015.svg"
    -- performParse "(unofficial) Spanish" "spa2015.svg"
    -- performParse "(unofficial) Portuguese" "prt2015.svg"
    -- performParse "(unofficial) Slavic" "sla2015.svg"
    -- performParse "(unofficial) East Asian Studies" "eas2015.svg"
    -- performParse "(unofficial) English" "eng2015.svg"
    -- performParse "(unofficial) History and Philosophy of Science" "hps2015.svg"
    -- performParse "(unofficial) History" "his2015.svg"
    -- performParse "(unofficial) Geography" "ggr2015.svg"
    -- performParse "(unofficial) Aboriginal" "abs2015.svg"
    -- performParse "(unofficial) German" "ger2015.svg"

parseDynamicSvg :: T.Text -> T.Text -> IO ()
parseDynamicSvg graphName graphContents = do
    runDb $ performParseFromMemory graphName graphContents True

-- | The starting point for parsing a graph with a given title and file
-- after removing the graph if it already exists.
performParse :: T.Text -- ^ The title of the graph.
             -> String -- ^ The filename of the file that will be parsed.
             -> SqlPersistM ()
performParse graphName inputFilename = do
    gPath <- liftIO graphPath
    deleteExistingGraph graphName
    liftIO . print $ "Parsing graph " ++ T.unpack graphName ++ " from file " ++ inputFilename
    graphFile <- liftIO $ T.readFile (gPath ++ inputFilename)
    performParseFromMemory graphName graphFile False

-- | Deletes the graph with the given name from the database if it exists.
deleteExistingGraph :: T.Text -> SqlPersistM ()
deleteExistingGraph graphName = do
  graphEnt :: (Maybe (Entity Graph)) <- selectFirst [GraphTitle ==. graphName] []
  case graphEnt of
    Just graph -> do
      let gId = entityKey graph
      deleteGraph gId
    Nothing -> pure ()

performParseFromMemory :: T.Text -- ^ The title of the graph
                       -> T.Text -- ^ Filename of the SVG to parse
                       -> Bool -- ^ True if SVG is dynamically generated
                       -> SqlPersistM ()
performParseFromMemory graphName graphSvg isDynamic = do
    key <- insertGraph graphName graphWidth graphHeight isDynamic
    let parsedGraph = parseSvg key graphSvg
    insertElements parsedGraph
        where (graphWidth, graphHeight) = parseSizeFromSvg graphSvg


-- * Parsing functions

-- | Parse the height and width dimensions from the SVG element, respectively,
-- and return them as a tuple.
parseSizeFromSvg :: T.Text -> (Double, Double)
parseSizeFromSvg graphSvg =
    let tags = filter (TS.isTagOpenName "svg") $ TS.parseTags graphSvg
        svgRoot = safeHead (TS.TagText T.empty) tags
    in (parseDouble "width" svgRoot, parseDouble "height" svgRoot)
    where parseDouble = parseAttr double

-- | Parse an SVG file (respresented as a Text) and return three
-- lists corresponding to different graph elements. (edges, nodes, text)
parseSvg :: GraphId -> T.Text -> ([Path], [Shape], [Text])
parseSvg key graphSvg =
    let tags = TS.parseTags graphSvg
        -- Need to remove any "defs" which Inkscape added
        defs = TS.partitions (TS.isTagOpenName "defs") tags
        tagsWithoutDefs =
            if null defs
            then
                tags
            else
                -- TODO: pull this out into a generic helper
                takeWhile (not . TS.isTagOpenName "defs") tags ++
                concatMap (dropWhile (not . TS.isTagCloseName "defs")) defs
    in parseGraph key tagsWithoutDefs

-- | This and the following functions traverse the raw SVG tags and return
-- three lists, each containing values corresponding to different graph elements
-- (edges, nodes, and text).
parseGraph ::  GraphId                 -- ^ The unique identifier of the graph.
           -> [Tag T.Text]             -- ^ The tags of the graph.
           -> ([Path],[Shape],[Text])
parseGraph key tags =
    let gTags = TS.partitions (TS.isTagOpenName "g") tags
        globalTransform = case gTags of
            ((x:_):_) -> getTransform x
            _ ->   [[1, 0, 0],
                    [0, 1, 0],
                    [0, 0, 1]]
        ellipses = concatMap (parseEllipse globalTransform key) gTags
        paths = concatMap (parsePath globalTransform key) gTags
        rects = concatMap (parseRect globalTransform key) gTags
        texts = concatMap (parseText globalTransform key) gTags
        shapes = removeRedundant (ellipses ++ rects)
    in
        (paths, filter small shapes, texts)
    where
        -- Raw SVG seems to have a rectangle the size of the whole image
        small shape = shapeWidth shape < 300
        removeRedundant shapes =
            filter (not . \s -> shapePos s `elem` map shapePos shapes &&
                                isEdge s &&
                                elem (shapeType_ s) [Node, Hybrid]) shapes

-- | Determine if the input shape is an edge.
isEdge :: Shape -> Bool
isEdge shape = T.null (shapeFill shape) || shapeFill shape == "black" || shapeFill shape == "#000000"

-- | Create text values from g tags.
-- This searches for nested tspan tags inside text tags using a recursive
-- helper function.
parseText :: Matrix -> GraphId -> [Tag T.Text] -> [Text]
parseText globalTrans key tags =
    let trans = case tags of
            [] -> [[1, 0, 0],
                    [0, 1, 0],
                    [0, 0, 1]]
            (x:_) -> getTransform x
        textTags = TS.partitions (TS.isTagOpenName "text") tags
        texts = concatMap (parseTextHelper key [] trans globalTrans) textTags
    in
        texts


parseTextHelper :: GraphId -- ^ The Text's corresponding graph identifier.
                -> [(T.Text, T.Text)]
                -> Matrix
                -> Matrix
                -> [Tag T.Text]
                -> [Text]
parseTextHelper _ _ _ _ [] = []
parseTextHelper key styles' trans globalTrans (headTag:restTags) =
    let [[a, c, e], [b, d, f], _] = completeTrans
       in [Text key
            (fromAttrib "id" headTag) -- TODO: Why are we setting an id?
            (readAttr "x" headTag, readAttr "y" headTag)
            (TS.escapeHTML $ trim $ TS.innerText (headTag:restTags))
            align
            fill
            [a, b, c, d, e, f]
       ]
    where
       newStyle = styles headTag ++ styles'
       currTrans = getTransform headTag
       newTrans = matrixMultiply trans currTrans
       completeTrans = matrixMultiply globalTrans newTrans
       alignAttr = styleVal "text-anchor" newStyle
       align = if T.null alignAttr
               then "start"
               else alignAttr
       fill = styleVal "fill" newStyle


-- | Create a rectangle from a list of attributes.
parseRect :: Matrix
          -> GraphId -- ^ The Rect's corresponding graph identifier.
          -> [Tag T.Text]
          -> [Shape]
parseRect _ _ [] = []
parseRect globalTrans key (tagsHead:tagsTail) =
    let
        rectOpenTags = filter (\tag -> TS.isTagOpenName "rect" tag || TS.isTagOpenName "polygon" tag) (tagsHead:tagsTail)
    in
        map (\tag -> if TS.isTagOpenName "rect" tag then makeRect tag else makePoly tag) rectOpenTags
    where
        styles' = styles tagsHead
        fill = styleVal "fill" styles'
        fill' = if T.null fill then fromAttrib "fill" tagsHead else fill
        trans = getTransform tagsHead
        completeTrans = matrixMultiply globalTrans trans
        makeRect rectOpenTag =
            let [[a, c, e], [b, d, f], _] = completeTrans
            in updateShape fill' $
                Shape key
                  ""
                  (readAttr "x" rectOpenTag, readAttr "y" rectOpenTag)
                  (readAttr "width" rectOpenTag)
                  (readAttr "height" rectOpenTag)
                  fill'
                  ""
                  []
                  Node
                  [a, b, c, d, e, f]
        makePoly polyOpenTag =
          let points = map (parseCoord . T.pack) $ splitOn " " $ T.unpack $ fromAttrib "points" polyOpenTag
              [[a, c, e], [b, d, f], _] = completeTrans
          in
            updateShape (fromAttrib "fill" polyOpenTag) $
              Shape key
                (fromAttrib "id" tagsHead)
                (points !! 1)
                (case points of
                    (xw:yw:_) -> fst xw - fst yw
                    _ -> 1.0) -- calculate width
                (case points of
                    (_:yh:zh:_) -> snd zh - snd yh
                    _ -> 1.0) -- calculate height
                fill
                ""
                []
                Node
                [a, b, c, d, e, f]


-- | Create a path from a list of tags.
parsePath :: Matrix
          -> GraphId
          -> [Tag T.Text]
          -> [Path]
parsePath globalTrans key tags =
    concatMap (parsePathHelper key trans globalTrans edgeInfo) (filter (TS.isTagOpenName "path") tags)
    where
        trans = case tags of
            [] -> [[1, 0, 0],
                    [0, 1, 0],
                    [0, 0, 1]]
            (x:_) -> getTransform x
        edgeInfo = case splitArr of
            [a, b] -> (T.pack a, T.pack b)
            _ -> ("", "")
            where splitArr = splitOn "|" (T.unpack (fromAttrib "id" $ safeHead (TS.TagOpen T.empty []) tags))


parsePathHelper :: GraphId -- ^ The Path's corresponding graph identifier.
                -> Matrix
                -> Matrix
                -> (T.Text, T.Text) -- src, dst
                -> Tag T.Text
                -> [Path]
parsePathHelper key trans globalTrans (src, dst) pathTag =
    let d = fromAttrib "d" pathTag
        styles' = styles pathTag
        currTrans = parseTransform $ fromAttrib "transform" pathTag
        newTrans = matrixMultiply trans currTrans
        completeTrans = matrixMultiply globalTrans newTrans
        [[a, c, e], [b, d', f], _] = completeTrans
        realD = parsePathD d
        fillAttr = styleVal "fill" styles'
        isRegion = not (T.null fillAttr) && fillAttr /= "none"
    in
        [updatePath fillAttr $
        Path key
            ""
            (removeDups realD)
            ""
            ""
            isRegion
            src
            dst
            [a, b, c, d', e, f] | not (T.null d || null realD || (T.last d == 'z' && not isRegion))
        ]
    where
        -- Remove consecutive duplicated points
        removeDups :: Eq a => [a] -> [a]
        removeDups [] = []
        removeDups [p] = [p]
        removeDups (a:b:c) =
            if a == b
                then removeDups (b:c)
                else a : removeDups (b:c)


-- | Create an ellipse from an open ellipse tag.
parseEllipse :: Matrix
             -> GraphId
             -> [Tag T.Text]
             -> [Shape]
parseEllipse globalTrans key tags =
    zipWith (parseEllipseHelper key trans globalTrans) (map fst ellipseGroups) (map getId ellipseGroups)
    where
        -- Group ellipses and optionally an enclosing g tags
        ellipseGroups :: [(Tag T.Text, Maybe (Tag T.Text))]
        ellipseGroups =
            map (\(t:ts) -> (t, List.find (TS.isTagOpenName "g") ts)) $
            TS.partitions (TS.isTagOpenName "ellipse") $
            List.reverse tags

        trans = case tags of
                [] -> [[1, 0, 0],
                        [0, 1, 0],
                        [0, 0, 1]]
                (x:_) -> getTransform x

        getId (t, Nothing) = fromAttrib "id" t
        getId (t1, Just t2) =
            if T.null $ fromAttrib "id" t1
            then
                fromAttrib "id" t2
            else
                fromAttrib "id" t1


parseEllipseHelper :: GraphId     -- ^ The related graph id.
                   -> Matrix      -- ^ The translation to apply.
                   -> Matrix      -- ^ The graph's global transformation.
                   -> Tag T.Text  -- ^ The open ellipse tag.
                   -> T.Text      -- ^ The id of the shape.
                   -> Shape
parseEllipseHelper key trans globalTrans ellipseTag id_ =
    let completeTrans = matrixMultiply globalTrans trans
        [[a, c, e], [b, d, f], _] = completeTrans
    in Shape key
          id_
          (readAttr "cx" ellipseTag, readAttr "cy" ellipseTag)
          (readAttr "rx" ellipseTag * 2)
          (readAttr "ry" ellipseTag * 2)
          ""
          ""
          []
          BoolNode
          [a, b, c, d, e, f]


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
parseValWithState :: P.Parsec String u a -> u -> T.Text -> a
parseValWithState parser initialState input =
    case P.runParser parser initialState "" $ T.unpack input of
        Left err ->
            error ("reading " ++ T.unpack input ++ ":" ++ show err)
        Right val -> val

-- | Runs a parser on a text object without internal parser state.
-- Throws an exception on any parse errors.
parseVal :: Parser a -> T.Text -> a
parseVal parser = parseValWithState parser ()

-- | Looks up an attribute value using the given parser.
parseAttr :: Parser a -> T.Text     -- ^ The attribute's name.
                      -> Tag T.Text -- ^ The element containing the attribute.
                      -> a
parseAttr parser attr tag = parseVal parser $ fromAttrib attr tag

-- | Parses one or more digit characters.
digits :: P.Parsec String u String
digits = P.many1 P.digit

-- | Parses a double literal, optionally in scientific notation.
double :: P.Parsec String u Double
double = do
    sign <- P.option ' ' (P.char '-')
    number <- wholeAndFractional
    magnitude <- P.option "" parseMagnitude
    return (read $ sign : number ++ magnitude)
    where
        wholeAndFractional = do
            whole <- digits <|> parseFractional
            fractional <- P.option "" parseFractional
            return $ case whole of
                [] -> []
                ('.':xs) -> '0' : ('.':xs)
                (x:xs) -> (x:xs) ++ fractional
        parseFractional = do
            _ <- P.char '.'
            decimals <- digits
            return ("." ++ decimals)
        parseMagnitude = do
            ch <- P.oneOf "eE"
            sign <- P.option "" $ P.string "-"
            power <- digits
            return (ch : sign ++ power)

point :: P.Parsec String u Point
point = do
    xPos <- double
    _ <- P.char ','
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
getTransform :: Tag T.Text -> Matrix
getTransform = parseTransform . fromAttrib "transform"


-- | Parses a translation String into a 2D array of double (Matrix type),
-- ignoring scaling and rotation.
parseTransform :: T.Text -> Matrix
parseTransform "" = [[1, 0, 0],
                     [0, 1, 0],
                     [0, 0, 1]]
parseTransform transform =
    parseVal parser transform
    where
        parser = P.try scale <|>
                  P.try rotate <|>
                  P.try translate <|>
                  P.try matrix <|>
                  P.try skewX <|>
                  P.try skewY
        scale = do
            _ <- P.string "scale("
            args <- double `P.sepBy` (P.char ',' *> P.many (P.char ' ') <|> P.many1 (P.char ' '))
            let xScale = safeHead 1 args
                yScale = if length args > 1 then args !! 1 else xScale
            return [[xScale, 0, 0],
                    [0, yScale, 0],
                    [0, 0, 1]]
        rotate = do
            _ <- P.string "rotate("
            args <- double `P.sepBy` (P.char ',' *> P.many (P.char ' ') <|> P.many1 (P.char ' '))
            let angleDegrees = safeHead 0 args
                xRot = if length args > 1 then args !! 1 else 0
                yRot = if length args > 2 then args !! 2 else 0
            let angle = angleDegrees * pi / 180
            return [[cos angle, - sin angle, xRot * (1 - cos angle) + yRot * sin angle],
                    [sin angle, cos angle, yRot * (1 - cos angle) - xRot * sin angle],
                    [0, 0, 1]]
        translate = do
            _ <- P.string "translate("
            args <- double `P.sepBy` (P.char ',' *> P.many (P.char ' ') <|> P.many1 (P.char ' '))
            let xPos = safeHead 0 args
                yPos = if length args > 1 then args !! 1 else 0
            return [[1, 0, xPos],
                    [0, 1, yPos],
                    [0, 0, 1]]
        matrix = do
            _ <- P.string "matrix("
            a <- double
            b <- (P.char ',' <|> P.char ' ') >> double
            c <- (P.char ',' <|> P.char ' ') >> double
            d <- (P.char ',' <|> P.char ' ') >> double
            e <- (P.char ',' <|> P.char ' ') >> double
            f <- (P.char ',' <|> P.char ' ') >> double
            return [[a, c, e],
                    [b, d, f],
                    [0, 0, 1]]
        skewX = do
            _ <- P.string "skewX("
            angleDegrees <- double
            let angle = angleDegrees * pi / 180
            return [[1, tan angle, 0],
                    [0, 1, 0],
                    [0, 0, 1]]
        skewY = do
            _ <- P.string "skewY("
            angleDegrees <- double
            let angle = angleDegrees * pi / 180
            return [[1, 0, 0],
                    [tan angle, 1, 0],
                    [0, 0, 1]]

parseCoord :: T.Text -> Point
parseCoord "" = (0, 0)
parseCoord coord =
    parseVal point coord


data PathMode = AbsoluteMove
              | RelativeMove
              | AbsoluteHorizontal
              | RelativeHorizontal
              | AbsoluteVertical
              | RelativeVertical

data PathDState = PathDState
    { firstPoint :: Point
    , mode :: PathMode
    , currentPoint :: Point }

-- | Parses a path's `d` attribute.
-- See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d>.
parsePathD :: T.Text -- ^ The 'd' attribute of an SVG path.
           -> [Point]
parsePathD = parseValWithState parser initialState
    where
        -- Keep track of the (first point, path mode, most recent point)
        initialState = PathDState { firstPoint = (0, 0)
                                  , mode = AbsoluteMove
                                  , currentPoint = (0, 0) }

        parser :: P.Parsec String PathDState [Point]
        parser = do
            p <- parseStep
            -- Record the first point added.
            P.modifyState $ \st -> st { firstPoint = p, currentPoint = p }
            rest <- P.many parseStep
            return (p : rest)

        parseStep :: P.Parsec String PathDState Point
        parseStep = do
            _ <- P.option () $ P.choice
                [ bezier
                , absoluteMove
                , relativeMove
                , absHorizontalMove
                , absVerticalMove
                , relHorizontalMove
                , relVerticalMove ]
            _ <- P.spaces
            arg <- closeLoop <|> moveArg
            _ <- P.spaces
            state <- P.getState
            let p = nextPoint arg state
            -- Every time a new point is added, update the parser state.
            P.modifyState $ \st -> st { currentPoint = p }
            return p

        -- Compute the next point given the input argument and current state.
        nextPoint :: Point -> PathDState -> Point
        nextPoint arg state =
            let (currentX, currentY) = currentPoint state
                (newX, newY) = arg
            in
                case mode state of
                    AbsoluteMove       -> arg
                    AbsoluteHorizontal -> (newX, currentY)
                    AbsoluteVertical   -> (currentX, newY)
                    RelativeMove       -> addTuples (currentPoint state) arg
                    RelativeHorizontal -> (currentX + newX, currentY)
                    RelativeVertical   -> (currentX, currentY + newY)

        bezier :: P.Parsec String PathDState ()
        bezier = -- Discard the next two points. TODO: Add full support for Bezier curves
            (P.char 'C' <|> P.char 'c') >>
            P.spaces >> point >>
            P.spaces >> point >>
            return ()

        closeLoop :: P.Parsec String PathDState Point
        closeLoop = do
            _ <- P.char 'Z' <|> P.char 'z'
            state <- P.getState
            _ <- setMode AbsoluteMove
            return (firstPoint state)

        moveArg :: P.Parsec String PathDState Point
        moveArg = do -- Read a movement argument (can be a point or a double).
            val <- double
            sep <- P.option ' ' $ P.char ','
            if sep == ',' then do
                yVal <- double
                return (val, yVal)
            else
                return (val, val)

        absoluteMove      :: P.Parsec String PathDState ()
        absoluteMove      = (P.char 'L' <|> P.char 'M') >> setMode AbsoluteMove
        relativeMove      :: P.Parsec String PathDState ()
        relativeMove      = (P.char 'l' <|> P.char 'm') >> setMode RelativeMove
        absHorizontalMove :: P.Parsec String PathDState ()
        absHorizontalMove = P.char 'H' >> setMode AbsoluteHorizontal
        absVerticalMove   :: P.Parsec String PathDState ()
        absVerticalMove   = P.char 'V' >> setMode AbsoluteVertical
        relHorizontalMove :: P.Parsec String PathDState ()
        relHorizontalMove = P.char 'h' >> setMode RelativeHorizontal
        relVerticalMove   :: P.Parsec String PathDState ()
        relVerticalMove   = P.char 'v' >> setMode RelativeVertical

        setMode :: PathMode -> P.Parsec String PathDState ()
        setMode newMode = P.modifyState $ \state -> state { mode = newMode }


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

-- | Apply a matrix transformation to a point.
-- The matrix must have dimensions 3x3, representing a transformation for a two-dimensional point,
-- i.e., the transformation in the z-component is ignored, so the third row is expected to be [0,0,1]
matrixPointMultiply :: Matrix -> Point -> Point
matrixPointMultiply matrix (x, y) =
    case matrix of
        [[a, b, tx], [c, d, ty], _] -> (a * x + b * y + tx, c * x + d * y + ty)
        _ -> error "Matrix must be 3x3 for point transformation."

-- | Multiplies two 3x3 matrices together.
matrixMultiply :: Matrix -> Matrix -> Matrix
matrixMultiply m1 m2 = [[dotProduct row col | col <- transpose m2] | row <- m1]

-- | Computes the dot product of two vectors.
dotProduct :: Vector -> Vector -> Double
dotProduct v1 v2 = sum $ zipWith (*) v1 v2

-- | Helper to remove leading and trailing whitespace.
trim :: T.Text -> T.Text
trim = f . f
    where f = T.reverse . T.dropWhile isSpace

-- | Extract an attribute, crashes if not a 'TagOpen'.
--   Returns @\"\"@ if no attribute present.
fromAttrib :: T.Text -> Tag T.Text -> T.Text
fromAttrib att (TS.TagOpen _ atts) = fromMaybe T.empty $ lookup att atts
fromAttrib _ x = error $ show x ++ " is not a TagText"
