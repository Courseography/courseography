{-# LANGUAGE OverloadedStrings #-}

module WebParsing.HtmlTable where

import qualified Data.Text as T
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Data.List
import Data.Maybe

type Pos = (Maybe Int, Maybe Int, Maybe Int, Maybe Int)

isSpan :: Tag T.Text -> Bool
isSpan =
    tagOpen (const True) isSpanAttr
    where
        isSpanAttr = any (\(attr, _) -> (attr == "colspan" || attr == "rowspan"))

-- | Returns the position of the first tag that contains rowspan or colspan attributes
-- the input is a HTML table interpreted by TagSoup, paritioned by 'td' tags
findSpans :: [[[Tag T.Text]]] -> [(Maybe Int, Maybe Int)]
findSpans rows =
    map (\r -> (Just r, getCol r)) rowpos
    where
        getCol rowpos = findIndex (isSpan . head ) (rows !! rowpos)
        rowpos = findIndices (foldl (\bool td -> bool || isSpan (head td)) False) rows

toInt :: Maybe T.Text -> Maybe Int
toInt Nothing = Nothing
toInt (Just text)
    | length maybeInt == 1 = Just $ fst (head maybeInt)
    | otherwise = Nothing
    where
        maybeInt = reads $ T.unpack text

maybeFromAttrib :: T.Text -> Maybe [Tag T.Text] -> Maybe T.Text
maybeFromAttrib _ Nothing = Nothing
maybeFromAttrib str (Just tag) = Just $ fromAttrib str (head tag)

-- | Finds the first rowspan and/or colspan, and returns a 4-element list where
-- * first element is the row it is found in, if any
-- * second element is the column it is found in, if any
-- * third element is the rowspan, if any
-- * fourth element is the colspan , if any
extractSpans :: [[[Tag T.Text]]] -> [Pos]
extractSpans cells  =
    map (getSpan cells) (findSpans cells)

getSpan ::  [[[Tag T.Text]]] -> (Maybe Int, Maybe Int) -> Pos
getSpan cells (row, col) =
    let cell = getCell cells (row, col)
        colspan = toInt $ maybeFromAttrib "colspan" cell
        rowspan = toInt $ maybeFromAttrib "rowspan" cell
    in (row, col, rowspan, colspan)

getCell :: [[[Tag T.Text]]] -> (Maybe Int, Maybe Int) -> Maybe [Tag T.Text]
getCell _ (Nothing, _) = Nothing
getCell _ (_, Nothing) = Nothing
getCell cells (Just rowpos, Just colpos)  = Just $ (cells !!rowpos) !! colpos

makeList :: a -> Int -> [a]
makeList value 1 = [value]
makeList value num = if num > 1
                     then value:makeList value (num - 1)
                     else []

-- | colpos is the position of the original colspanned cell
expandRow :: [a] -> a -> Maybe Int -> Maybe Int ->  [a]
expandRow row _ Nothing _ = row
expandRow row _ _ Nothing = row
expandRow row value (Just colpos) (Just colspan)
    | colpos > length row = row
    | otherwise =
          before ++ makeList value colspan ++ after
    where
        (before, after) = splitAt colpos row

expandRows :: [[a]] -> a -> Maybe Int -> Maybe Int -> Maybe Int -> [[a]]
expandRows [] _ _ _ _ =  []
expandRows rows value colpos colspan num
    | maybe False (<= 0) num || maybe False (\n-> n > length rows) num
        = rows
    | otherwise =
        let expanded = expandRow (head rows) value colpos colspan
        in expanded:expandRows (tail rows) value colpos colspan (maybeAdd num (-1))

expandTable :: [[a]] -> a -> Pos -> [[a]]
expandTable cells _ (Nothing, _, _, _) = cells
expandTable cells value (rowpos, colpos, Nothing, colspan)
    | maybe False (\n-> n > length cells) rowpos = cells
    | otherwise =
        let (before, restRows) = splitAt (fromJust rowpos) cells
            firstRow = expandRow (head restRows) value (maybeAdd colpos 1) (maybeAdd colspan (-1))
        in before ++ firstRow:tail restRows
expandTable cells value (rowpos, colpos, rowspan, colspan)
    | maybe False (\n-> n > length cells) rowpos ||
      maybe False (<= 0) rowspan = cells
    | otherwise =
        let (before, restRows) = splitAt (fromJust rowpos) cells
            firstRow = expandRow (head restRows) value (maybeAdd colpos 1) (maybeAdd colspan (-1))
            expanded = expandRows (tail restRows) value colpos colspan (maybeAdd rowspan (-1))
        in before ++ firstRow:expanded

maybeAdd :: Maybe Int -> Int -> Maybe Int
maybeAdd Nothing _ = Nothing
maybeAdd (Just x) y = Just (x + y)
