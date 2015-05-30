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
                []
                (do
                    header "draw"
                    drawHtml
                    modePanel
                )
                drawScripts


drawHtml :: H.Html
drawHtml = createTag H.div "about-div" "" "Draw a Graph"


modePanel :: H.Html
modePanel = createTag H.div "side-panel-wrap" "" $ do
    createTag H.div "node-mode" "mode clicked" "NODE (n)"
    H.input ! A.id "course-code" ! A.class_ "course-code" ! A.name "course-code" ! A.placeholder "Course Code" ! A.autocomplete "off" ! A.type_ "text" ! A.size "10"
    createTag H.div "add-text" "button" "ADD"
    createTag H.div "red" "colour clicked" "RED"
    createTag H.div "green" "colour" "GREEN"
    createTag H.div "blue" "colour" "BLUE"
    createTag H.div "purple" "colour" "PURPLE"
    createTag H.div "path-mode" "mode" "PATH (p)"
    createTag H.div "region-mode" "mode" "REGION (r)"
    createTag H.div "finish-region" "button" "finish (f)"
    createTag H.div "change-mode" "mode" "SELECT/MOVE (m)"
    createTag H.div "erase-mode" "mode" "ERASE (e)"