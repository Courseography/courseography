{-# LANGUAGE OverloadedStrings #-}

module Response.Export
    (exportResponse) where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server

exportResponse :: ServerPart Response
exportResponse =
    ok $ toResponse $
        displayPDF


{- Insert a large loading icon into the page -}
displayPDF :: H.Html
displayPDF =
    H.div ! A.id "exportDiv" $
        H.img ! A.id "export-pdf" ! A.src "../CSgraph.pdf"
