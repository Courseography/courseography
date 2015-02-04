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
  createTag H.div "node-mode" "mode clicked" "NODE"
  createTag H.div "red" "colour clicked" "RED"
  createTag H.div "green" "colour" "GREEN"
  createTag H.div "blue" "colour" "BLUE"
  createTag H.div "purple" "colour" "PURPLE"
  createTag H.div "erase-mode" "mode" "ERASE" 
  createTag H.div "change-mode" "mode" "MODIFY" 
