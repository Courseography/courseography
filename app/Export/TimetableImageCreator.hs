{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

module Export.TimetableImageCreator
    (renderTable, renderTableHelper, times) where

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
cellText s = font "Trebuchet MS" $ text s # fontSizeO fs

makeCell :: Int -> String -> Diagram B
makeCell maxCourse s = 
    let sList = splitOn "&" s
        actualCourse = length sList
        extraCell = replicate (maxCourse - actualCourse) [cellPadding # fc white # lc white, cellText "" # fc white <> cell # fc white # lc white]
    in vsep 0.030 $
        concat $ map (\x -> [cellPadding # fc background # lc background, cellText x # fc white <> cell # fc background # lc background]) sList ++ extraCell
    where
        background = getBackground s

getBackground :: String -> Colour Double
getBackground s
    | null s = white
    | elem '&' s = pomegranate
    | otherwise = blue3

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

makeRow :: [String] -> Diagram B
makeRow (x:xs) = 
    let maxCourse = maximum (map (length . (splitOn "&")) xs)
    in (# centerX) . hcat $ 
        makeTimeCell x : map (makeCell maxCourse) xs
makeRow [] = error "invalid timetable format"

headerBorder :: Diagram B
headerBorder = hrule 11.2 # lw medium # lc pink1

rowBorder :: Diagram B
rowBorder = hrule 11.2 # lw thin # lc pink1

makeTable :: [[String]] -> String -> Diagram B
makeTable s session = vsep 0.04 $ (header session): intersperse rowBorder (map makeRow s)

renderTable :: String -> String -> String -> IO ()
renderTable filename courses session = do
    let courseTable = partition5 $ splitOn "_" courses
    renderTableHelper filename (zipWith (:) times courseTable) session
    where
        partition5 [] = []
        partition5 lst = take 5 lst : partition5 (drop 5 lst)


-- =====================================================

-- coursetable : [["","","","",""],["","","","",""],["CSC108 (L)","","CSC108 (L)","","CSC108 (L)"],["","","","",""],["","","","",""],["","","","",""],["STA355 (L)","","STA355 (L)","",""],["STA355 (L)","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],[""]]
-- zipwith :  [["8:00","","","","",""],["9:00","","","","",""],["10:00","CSC108 (L)","","CSC108 (L)","","CSC108 (L)"],["11:00","","","","",""],["12:00","","","","",""],["1:00","","","","",""],["2:00","STA355 (L)","","STA355 (L)","",""],["3:00","STA355 (L)","","","",""],["4:00","","","","",""],["5:00","","","","",""],["6:00","","","","",""],["7:00","","","","",""],["8:00","","","","",""]]

renderTableHelper :: String -> [[String]] -> String -> IO ()
renderTableHelper filename schedule session = do
    let g = makeTable schedule session
        svg = renderDia SVG (SVGOptions (mkWidth 1024) Nothing "") g
        txt = replace (show (fs :: Double) ++ "px") (show fs' ++ "px") $
              unpack $ renderText svg
    writeFile filename txt
    where
        -- relative fonts don't play well with ImageMagick, apparently
        fs' = round $ 1024 / 800 * fs