{-# LANGUAGE OverloadedStrings #-}

module DrawResponse where
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MakeElements
import MasterTemplate
import Scripts

drawResponse :: ServerPart Response
drawResponse =
   ok $ toResponse $
    masterTemplate "Courseography - Draw!"
                [H.meta ! A.name "keywords"
                        ! A.content "",
                 drawLinks
                ]
                (do
                    header "draw"
                    drawHtml

                    modePanel
                )
                timetableScripts


drawHtml :: H.Html
drawHtml = H.div ! A.id "aboutDiv" $ do
  H.h2 "Draw a Graph"

modePanel :: H.Html
modePanel = createTag H.div "mode-panel" "" $ do 
  createTag H.div "node-mode" "mode" "NODE"
  createTag H.ul "colour-select" "" $ do
    createTag H.li "red" "colour" "RED"
    createTag H.li "green" "colour" "GREEN"
    createTag H.li "blue" "colour" "BLUE"
    createTag H.li "purple" "colour" "PURPLE"
  createTag H.div "erase-mode" "mode" "ERASE" 

