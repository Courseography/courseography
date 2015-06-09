{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

module Diagram (renderTable) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG
import Data.List (intersperse)
import Data.List.Utils (replace)
import Data.List.Split (splitOn)
import Lucid (renderText)
import Data.Text.Lazy (unpack)

blue3 :: Colour Double
blue3 = sRGB24read "#437699"

pink1 :: Colour Double
pink1 = sRGB24read "#DB94B8"

days :: [String]
days = ["Mon", "Tue", "Wed", "Thu", "Fri"]

times :: [String]
times = map (\x -> show x ++ ":00") ([8..12] ++ [1..8] :: [Int])

cell :: Diagram B
cell = rect 2 0.4

cellPadding :: Diagram B
cellPadding = rect 2 0.2

makeCell :: String -> Diagram B
makeCell s = vsep 0.025
    [(cellPadding # fc (if null s then white else blue3)
                  # lc (if null s then white else blue3)),
    (font "Trebuchet MS" $ text s # fontSizeO 16 # fc white) <>
    cell # fc (if null s then white else blue3)
         # lc (if null s then white else blue3)]

header :: String -> Diagram B
header session = (hcat $ (makeSessionCell session) : map makeHeaderCell days) # centerX === headerBorder

headerBorder :: Diagram B
headerBorder = hrule 11.2 # lw medium # lc pink1

makeSessionCell :: String -> Diagram B
makeSessionCell s = vcat
    [rect 1.2 0.2 # lw none,
     (font "Trebuchet MS" $ text s # fontSizeO 16) <>
     rect 1.2 0.4 # lw none]

makeHeaderCell :: String -> Diagram B
makeHeaderCell s = vcat
    [cellPadding # lw none,
    (font "Trebuchet MS" $ text s # fontSizeO 16) <> cell # lw none]

makeTimeCell :: String -> Diagram B
makeTimeCell s = vcat
    [rect 1.2 0.2 # lw none,
     (font "Trebuchet MS" $ text s # fontSizeO 16) <>
     rect 1.2 0.4 # lw none]

makeRow :: [String] -> Diagram B
makeRow (x:xs) = (# centerX) . hcat $
    makeTimeCell x : map makeCell xs
makeRow [] = error "invalid timetable format"

rowBorder :: Diagram B
rowBorder = hrule 11.2 # lw thin # lc pink1

makeTable :: [[String]] -> String -> Diagram B
makeTable s session = vsep 0.04 $ (header session): intersperse rowBorder (map makeRow s)

renderTable :: String -> String -> String -> IO ()
renderTable filename courses session = do
    let courseTable = partition5 $ splitOn "_" courses
    print courseTable
    let g = makeTable (zipWith (:) times courseTable) session
    let svg = renderDia SVG (SVGOptions (mkWidth 600) Nothing "") g
    let txt = replace "16.0em" "16.0px" $ unpack $ renderText svg
    writeFile filename txt
    where
        partition5 [] = []
        partition5 lst = take 5 lst : partition5 (drop 5 lst)
