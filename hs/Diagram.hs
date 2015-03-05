{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagram (renderTable) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG
import Data.List

days :: [String]
days = ["Mon", "Tue", "Wed", "Thu", "Fri"]

times :: [String]
times = map (\x -> show x ++ ":00") ([8..12] ++ [1..8])

cell :: Diagram B R2
cell = rect 2 0.8

makeCell :: String -> Diagram B R2
makeCell s = 
    (font "Trebuchet MS" $ text s # fontSizeN 0.03 # fc white) <>
    cell # fc (if null s then white else blue)
         # lw none

header :: Diagram B R2
header = (hcat $ map makeHeaderCell $ "Fall":days) # centerX === headerBorder

headerBorder :: Diagram B R2
headerBorder = hrule 12 # lw medium # lc pink

makeHeaderCell :: String -> Diagram B R2
makeHeaderCell s = 
    (font "Trebuchet MS" $ text s # fontSizeN 0.03) <>
    cell # lw none

makeTimeCell :: String -> Diagram B R2
makeTimeCell s = 
    (font "Trebuchet MS" $ text s # fontSizeN 0.03) <>
    cell # lw none

makeRow :: [String] -> Diagram B R2
makeRow (x:xs) = (# centerX) . hcat $ 
    makeTimeCell x : vrule 0.8 # lw thin : map makeCell xs

rowBorder :: Diagram B R2
rowBorder = hrule 12 # lw thin # lc grey

makeTable :: [[String]] -> Diagram B R2
makeTable s = vcat $ header : intersperse rowBorder (map makeRow s)

renderTable :: String -> String -> IO ()
renderTable filename courses = do
    let courseTable = partition5 $ lines courses
    print courseTable
    let g = makeTable $ zipWith (:) times courseTable
    renderSVG filename (Width 600) g
    where
        partition5 [] = []
        partition5 lst = take 5 lst : partition5 (drop 5 lst)
