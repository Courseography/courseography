{-# LANGUAGE OverloadedStrings #-}

module DrawResponse where
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server -- need?
import MakeElements
import MasterTemplate

drawResponse :: ServerPart Response
drawResponse =
   ok $ toResponse $
    masterTemplate "Courseography - SVG serving test!"
                [H.meta ! A.name "keywords"
                        ! A.content "",
                 aboutLinks
                ]
                (do
                    header "draw"
                    drawHtml
                )
                ""


drawHtml :: H.Html
drawHtml = H.div ! A.id "aboutDiv" $ do
  H.h1 "Draw a Graph"
  H.p $ do
    "Here you can create a graph yourself!"