{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

module Diagram (renderTable) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG
import Data.List
import Data.List.Utils (replace)
import Data.List.Split (splitOn)
import Lucid (renderText)
import Data.Text.Lazy (unpack)

days :: [String]
days = ["Mon", "Tue", "Wed", "Thu", "Fri"]

times :: [String]
times = map (\x -> show x ++ ":00") ([8..12] ++ [1..8] :: [Int])

cell :: Diagram B
cell = rect 2 0.8

makeCell :: String -> Diagram B
makeCell s =
    (font "Trebuchet MS" $ text s # fontSizeO 16 # fc white) <>
    cell # fc (if null s then white else blue)
         # lw none

header :: String -> Diagram B
header session = (hcat $ map makeHeaderCell $ session:days) # centerX === headerBorder

headerBorder :: Diagram B
headerBorder = hrule 12 # lw medium # lc pink

makeHeaderCell :: String -> Diagram B
makeHeaderCell s =
    (font "Trebuchet MS" $ text s # fontSizeO 16) <>
    cell # lw none

makeTimeCell :: String -> Diagram B
makeTimeCell s =
    (font "Trebuchet MS" $ text s # fontSizeO 16) <>
    cell # lw none

makeRow :: [String] -> Diagram B
makeRow (x:xs) = (# centerX) . hcat $
    makeTimeCell x : vrule 0.8 # lw thin : map makeCell xs

rowBorder :: Diagram B
rowBorder = hrule 12 # lw thin # lc grey

makeTable :: [[String]] -> String -> Diagram B
makeTable s session = vcat $ (header session): intersperse rowBorder (map makeRow s)

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
