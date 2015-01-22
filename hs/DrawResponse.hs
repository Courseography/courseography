{-# LANGUAGE OverloadedStrings #-}

module DrawResponse where
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server -- need?
import MakeElements
import MasterTemplate
import Scripts

drawResponse :: ServerPart Response
drawResponse =
   ok $ toResponse $
    masterTemplate "Courseography - Draw!"
                [H.meta ! A.name "keywords"
                        ! A.content "",
                 timetableLinks
                ]
                (do
                    header "draw"
                    drawHtml

                    button
                    --modePanel
                    theCanvas
                )
                timetableScripts


drawHtml :: H.Html
drawHtml = H.div ! A.id "aboutDiv" $ do
  H.h1 "Draw a Graph"
  H.p "Here you can create a graph yourself!"

button :: H.Html
button = createTag H.div "target" "" $ do "Click Here"

--modePanel :: H.Html
--modePanel = createTag H.div "mode-select" "col-md-2 col-xs-6" $ do "HAHA"

-- <canvas id="myCanvas" width="200" height="100"></canvas>
theCanvas :: H.Html
theCanvas = H.canvas ! A.id "theCanvas" ! A.width "800" ! A.height"375" $ do "No canvas support."

