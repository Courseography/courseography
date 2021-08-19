{-|
    Module      : Export.TimetableImageCreator
    Description : Primarily defines a function used to render SVGs with times.
-}
module Export.TimetableImageCreator
    (renderTable, renderTableHelper, times) where

import Data.List (intersperse)
import qualified Data.Text as T
import Diagrams.Backend.SVG
import Diagrams.Prelude

days :: [T.Text]
days = ["Mon", "Tue", "Wed", "Thu", "Fri"]

-- |A list of lists of Texts, which has the "times" from 8:00 to 12:00, and
-- 1:00 to 8:00.times
times :: [[T.Text]]
times = map (\x -> [T.pack (show x ++ ":00")]) ([8..12] ++ [1..8] :: [Int])

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

cellText :: T.Text -> Diagram B
cellText s = font "Trebuchet MS" $ text (T.unpack s) # fontSizeO (1024/900 * fs)

-- | Creates and accumulates cells according to the number of course.
makeCell :: Int -> [T.Text] -> Diagram B
makeCell maxCourse sList =
    let actualCourse = length sList
        emptyCellNum = if maxCourse == 0 then 1 else maxCourse - actualCourse
        extraCell = replicate emptyCellNum [cellPadding # fc white # lc white, cellText "" # fc white <> cell # fc white # lc white]
    in vsep 0.030 $
        concat $ map (\x -> [cellPadding # fc background # lc background, cellText x # fc white <> cell # fc background # lc background]) sList ++ extraCell
    where
        background = getBackground sList

getBackground :: [T.Text] -> Colour Double
getBackground s
    | null s = white
    | length s == 1 = blue3
    | otherwise = pomegranate

header :: T.Text -> Diagram B
header session = hcat (makeSessionCell session : map makeHeaderCell days) # centerX === headerBorder

makeSessionCell :: T.Text -> Diagram B
makeSessionCell s =
    timeCellPadding === (cellText s <> timeCell)

makeHeaderCell :: T.Text -> Diagram B
makeHeaderCell s =
    (cellPadding # lw none # fc white # lc white) === (cellText s <> cell # lw none)

makeTimeCell :: T.Text -> Diagram B
makeTimeCell s =
    timeCellPadding === (cellText s <> timeCell)

makeRow :: [[T.Text]] -> Diagram B
makeRow ([x]:xs) =
    let maxCourse = maximum (map length xs)
    in (# centerX) . hcat $
        makeTimeCell x : map (makeCell maxCourse) xs
makeRow _ = error "invalid timetable format"

headerBorder :: Diagram B
headerBorder = hrule 11.2 # lw medium # lc pink1

rowBorder :: Diagram B
rowBorder = hrule 11.2 # lw thin # lc pink1

makeTable :: [[[T.Text]]] -> T.Text -> Diagram B
makeTable s session = vsep 0.04 $ header session: intersperse rowBorder (map makeRow s)

-- |Creates a timetable by zipping the time and course tables.
renderTable :: String -> T.Text -> T.Text -> IO ()
renderTable filename courses session = do
    let courseTable = partition5 $ map (\x -> [x | not (T.null x)]) $ T.splitOn "_" courses
    renderTableHelper filename (zipWith (:) times courseTable) session
    where
        partition5 [] = []
        partition5 lst = take 5 lst : partition5 (drop 5 lst)

-- |Renders an SVG with a width of 1024, though the documentation doesn't
-- specify the units, it is assumed that these are pixels.
renderTableHelper :: String -> [[[T.Text]]] -> T.Text -> IO ()
renderTableHelper filename schedule session = do
    let g = makeTable schedule session
    renderSVG filename (mkWidth 1024) g
