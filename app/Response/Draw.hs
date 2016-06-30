{-# LANGUAGE OverloadedStrings #-}

module Response.Draw
    (drawResponse) where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T
import Control.Monad (forM_, mapM_)
import Happstack.Server
import MasterTemplate
import Scripts

drawResponse :: ServerPart Response
drawResponse =
   ok $ toResponse $
    masterTemplate "Courseography - Draw!"
                []
                (do
                    header "draw"
                    drawContent
                    modePanel
                )
                drawScripts

drawContent :: H.Html
drawContent = H.div ! A.id "about-div" $ "Draw a Graph"

modePanel :: H.Html
modePanel = H.div ! A.id "side-panel-wrap" $ do
    H.div ! A.id "node-mode" ! A.class_ "mode clicked" $ "NODE (n)"
    H.input ! A.id "course-code"
            ! A.class_ "course-code"
            ! A.name "course-code"
            ! A.placeholder "Course Code"
            ! A.autocomplete "off"
            ! A.type_ "text"
            ! A.size "10"
    H.div ! A.id "add-text" ! A.class_ "button" $ "ADD"
    H.div ! A.id "path-mode" ! A.class_ "mode" $ "PATH (p)"
    H.div ! A.id "region-mode" ! A.class_ "mode" $ "REGION (r)"
    H.div ! A.id "finish-region" ! A.class_ "button" $ "finish (f)"
    H.div ! A.id "change-mode" ! A.class_ "mode" $ "SELECT/MOVE (m)"
    H.div ! A.id "erase-mode" ! A.class_ "mode" $ "ERASE (e)"
    H.input ! A.id "select-colour"
            ! A.class_ "jscolor"
            ! A.value "ff7878" -- pastelRed color as defined in Css.Constants
            ! A.size "15"
    H.table !A.id "colour-table" $ forM_ (replicate 2 $ replicate 5 "" :: [[H.Html]])
                                         (H.tr . mapM_ (H.td . H.toHtml))
    H.div ! A.id "save-graph" ! A.class_ "button" $ "SAVE"
    H.input ! A.id "area-of-study"
            ! A.class_ "course-code"
            ! A.name "course-code"
            ! A.placeholder "Enter area of study."
            ! A.autocomplete "off"
            ! A.type_ "text"
            ! A.size "30"
    H.div ! A.id "submit-graph-name" ! A.class_ "button" $ "Submit"
    H.div ! A.id "json-data" ! A.class_ "json-data" $ ""
