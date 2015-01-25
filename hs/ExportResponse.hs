{-# LANGUAGE OverloadedStrings #-}

module ExportResponse where
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MakeElements
import MasterTemplate
import Graphics.PDF

exportResponse :: ServerPart Response
exportResponse =
   ok $ toResponse $
    masterTemplate "Courseography - SVG serving test!"
                [H.meta ! A.name "keywords"
                        ! A.content "",
                 aboutLinks
                ]
                (do
                    header "export"
                    exportHtml
                )
                ""

testImage :: H.Html
testImage = H.img ! A.src "http://i.imgur.com/iYeoNwC.jpg" ! A.alt "Lady!"

exportHtml :: H.Html
exportHtml = H.div ! A.id "exportDiv" $ do
  H.h1 "Hello, world!"
  H.p $ do
    "Hello. Is anyone there? Here is a picture of my cat, Lady. Isn't she cute?"
  H.p $ do
    testImage
