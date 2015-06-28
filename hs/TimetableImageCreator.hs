{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

module TimetableImageCreator
    (renderTable) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG
import Data.List (intersperse)
import Data.List.Utils (replace)
import Data.List.Split (splitOn)
import Lucid (renderText)
import Data.Text.Lazy (unpack)

days :: [String]
days = ["Mon", "Tue", "Wed", "Thu", "Fri"]

times :: [String]
times = map (\x -> show x ++ ":00") ([8..12] ++ [1..8] :: [Int])

blue3 :: Colour Double
blue3 = sRGB24read "#437699"

pink1 :: Colour Double
pink1 = sRGB24read "#DB94B8"

cellWidth :: Double
cellWidth = 2

timeCellWidth :: Double
timeCellWidth = 1.2

cellHeight :: Double
cellHeight = 0.4

cellPaddingHeight :: Double
cellPaddingHeight = 0.2

fs :: Double
fs = 16

cell :: Diagram B
cell = rect cellWidth cellHeight

cellPadding :: Diagram B
cellPadding = rect cellWidth cellPaddingHeight

timeCell :: Diagram B
timeCell = rect timeCellWidth cellHeight # lw none

timeCellPadding :: Diagram B
timeCellPadding = rect timeCellWidth cellPaddingHeight # lw none

cellText :: String -> Diagram B
cellText s = font "Trebuchet MS" $ text s # fontSizeO fs

makeCell :: String -> Diagram B
makeCell s = vsep 0.025
    [cellPadding # fc background # lc background,
     cellText s # fc white <>
     cell # fc background # lc background]
    where
        background = if null s then white else blue3

header :: String -> Diagram B
header session = (hcat $ (makeSessionCell session) : map makeHeaderCell days) # centerX === headerBorder

makeSessionCell :: String -> Diagram B
makeSessionCell s =
    timeCellPadding === (cellText s <> timeCell)

makeHeaderCell :: String -> Diagram B
makeHeaderCell s =
    cellPadding # lw none === (cellText s <> cell # lw none)

makeTimeCell :: String -> Diagram B
makeTimeCell s =
    timeCellPadding === (cellText s <> timeCell # lw none)

makeRow :: [String] -> Diagram B
makeRow (x:xs) = (# centerX) . hcat $
    makeTimeCell x : map makeCell xs
makeRow [] = error "invalid timetable format"

headerBorder :: Diagram B
headerBorder = hrule 11.2 # lw medium # lc pink1

rowBorder :: Diagram B
rowBorder = hrule 11.2 # lw thin # lc pink1

makeTable :: [[String]] -> String -> Diagram B
makeTable s session = vsep 0.04 $ (header session): intersperse rowBorder (map makeRow s)

renderTable :: String -> String -> String -> IO ()
renderTable filename courses session =
    do
        let courseTable = partition5 $ splitOn "_" courses
        print courseTable
        let g = makeTable (zipWith (:) times courseTable) session
            svg = renderDia SVG (SVGOptions (mkWidth 1024) Nothing "") g
            txt = replace (show (fs :: Double) ++ "px") (show fs' ++ "px") $
                  unpack $ renderText svg
        writeFile filename txt
        where
            partition5 [] = []
            partition5 lst = take 5 lst : partition5 (drop 5 lst)

            -- relative fonts don't play well with ImageMagick, apparently
            fs' = round $ 1024 / 600 * fs
