{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Data.Graph.Inductive
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Types.Generalised   as G
import           Data.GraphViz.Types.Monadic
import           Data.Text.Lazy                    as L
import           Data.Word
import           WriteRunDot


ex3 :: G.DotGraph L.Text
ex3 = digraph (Str "ex3") $ do

    graphAttrs [RankDir FromLeft]

    cluster (Str "G1") $ do
        nodeAttrs               [shape Circle, Width 1, style filled, myColor 1]
        node "A"                [textLabel "MAT237"]

    cluster (Str "G2") $ do
        nodeAttrs               [shape Circle, Width 1, style filled, myColor 1]
        node "B"                [textLabel "STA261"]

    cluster (Str "G3") $ do
        nodeAttrs               [shape Circle, Width 1, style filled, myColor 1]
        node "C"                [textLabel "MAT224"]

    cluster (Str "G4") $ do
        nodeAttrs               [shape Circle, Width 1, style filled, myColor 2]
        node "D"                [textLabel "MAT137"]

    cluster (Str "G5") $ do
        nodeAttrs               [shape Circle, Width 1, style filled, myColor 2]
        node "E"                [textLabel "MAT157"]

    cluster (Str "G6") $ do
        nodeAttrs               [shape Circle, Width 1, style filled, myColor 3]
        node "F"                [textLabel "STA257"]

    cluster (Str "G0") $ do
        nodeAttrs               [shape Circle, Width 1, style filled, myColor 4]
        node "ZZ"               [textLabel "STA302"]

    "A"             --> "ZZ"
    "B"             --> "ZZ"
    "C"             --> "ZZ"
    "D"             --> "Or"
    "E"             --> "Or"
    "Or"            --> "A"
    "F"             --> "B"



-- http://www.colorcombos.com/color-schemes/2025/ColorCombo2025.html
myColorCL :: Word8 -> ColorList
myColorCL n | n == 1 = c $ (RGB 127 108 138)
            | n == 2 = c $ (RGB 175 177 112)
            | n == 3 = c $ (RGB 226 206 179)
            | n == 4 = c $ (RGB 172 126 100)
 where c rgb = toColorList [rgb]

myColor :: Word8 -> Attribute
myColor n = Color $ myColorCL n



main :: IO ()
main = do
    doDots [ ("ex3" , ex3) ]