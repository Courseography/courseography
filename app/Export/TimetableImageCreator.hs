{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

{-|
    Module      : Export.TimetableImageCreator
    Description : Primarily defines a function used to render SVGs with times.
-}
module Export.TimetableImageCreator
    (renderTable, renderTableHelper, times) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG
import Data.List (intersperse)
import Data.List.Split (splitOn)

days :: [String]
days = ["Mon", "Tue", "Wed", "Thu", "Fri"]

times :: [[String]]
times = map (\x -> [show x ++ ":00"]) ([8..12] ++ [1..8] :: [Int])

blue3 :: Colour Double
blue3 = sRGB24read "#437699"

pink1 :: Colour Double
pink1 = sRGB24read "#DB94B8"

pomegranate :: Colour Double
pomegranate = sRGB24read "#F20C00"

cellWidth :: Double
cellWidth = 2

timeCellWidth :: Double
timeCellWidth = 1.2

cellHeight :: Double
cellHeight = 0.4

cellPaddingHeight :: Double
cellPaddingHeight = 0.1

fs :: Double
fs = 14

cell :: Diagram B
cell = rect cellWidth cellHeight

cellPadding :: Diagram B
cellPadding = rect cellWidth cellPaddingHeight

timeCell :: Diagram B
timeCell = rect timeCellWidth cellHeight # lw none

timeCellPadding :: Diagram B
timeCellPadding = rect timeCellWidth cellPaddingHeight # lw none

cellText :: String -> Diagram B
cellText s = font "Trebuchet MS" $ text s # fontSizeO (1024/900 * fs)

-- | Creates and accumulates cells according to the number of course.
makeCell :: Int -> [String] -> Diagram B
makeCell maxCourse sList =
    let actualCourse = length sList
        emptyCellNum = if maxCourse == 0 then 1 else maxCourse - actualCourse
        extraCell = replicate emptyCellNum [cellPadding # fc white # lc white, cellText "" # fc white <> cell # fc white # lc white]
    in vsep 0.030 $
        concat $ map (\x -> [cellPadding # fc background # lc background, cellText x # fc white <> cell # fc background # lc background]) sList ++ extraCell
    where
        background = getBackground sList

getBackground :: [String] -> Colour Double
getBackground s
    | null s = white
    | length s == 1 = blue3
    | otherwise = pomegranate

header :: String -> Diagram B
header session = (hcat $ (makeSessionCell session) : map makeHeaderCell days) # centerX === headerBorder

makeSessionCell :: String -> Diagram B
makeSessionCell s =
    timeCellPadding === (cellText s <> timeCell)

makeHeaderCell :: String -> Diagram B
makeHeaderCell s =
    (cellPadding # lw none # fc white # lc white) === (cellText s <> cell # lw none)

makeTimeCell :: String -> Diagram B
makeTimeCell s =
    timeCellPadding === (cellText s <> timeCell)

makeRow :: [[String]] -> Diagram B
makeRow ([x]:xs) =
    let maxCourse = maximum (map length xs)
    in (# centerX) . hcat $
        makeTimeCell x : map (makeCell maxCourse) xs
makeRow [] = error "invalid timetable format"

headerBorder :: Diagram B
headerBorder = hrule 11.2 # lw medium # lc pink1

rowBorder :: Diagram B
rowBorder = hrule 11.2 # lw thin # lc pink1

makeTable :: [[[String]]] -> String -> Diagram B
makeTable s session = vsep 0.04 $ (header session): intersperse rowBorder (map makeRow s)

renderTable :: String -> String -> String -> IO ()
renderTable filename courses session = do
    let courseTable = partition5 $ map (\x -> if null x then [] else [x]) $ splitOn "_" courses
    renderTableHelper filename (zipWith (:) times courseTable) session
    where
        partition5 [] = []
        partition5 lst = take 5 lst : partition5 (drop 5 lst)

renderTableHelper :: String -> [[[String]]] -> String -> IO ()
renderTableHelper filename schedule session = do
    let g = makeTable schedule session
    renderSVG filename (mkWidth 1024) g
