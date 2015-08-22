{-# LANGUAGE OverloadedStrings #-}

module Response.Grid
    (gridResponse) where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MasterTemplate
import Scripts

gridResponse :: ServerPart Response
gridResponse =
    ok $ toResponse $
        masterTemplate "Courseography - Grid"
            []
            (do header "grid"
                conflictDialog
                H.div ! A.class_ "row main" $ do
                     coursePanel
                     searchPanel
                     infoPanel
                disclaimer
            )
            timetableScripts


conflictDialog :: H.Html
conflictDialog =
    H.div ! A.id "dialog" $
        "Conflicting courses are difficult to manage. Make sure you understand the added responsibility of having two or more conflicting courses."

coursePanel :: H.Html
coursePanel =
    H.div ! A.id "course-select-wrapper" ! A.class_ "col-md-2 col-xs-6" $
        H.ul ! A.id "course-select" ! A.class_ "trapScroll-enabled" $
            H.li ! A.id "clear-all" $
                H.h3 "Clear All"

searchPanel :: H.Html
searchPanel =
    H.div ! A.id "search-layout" ! A.class_ "col-md-2 col-xs-6 col-md-push-8" $ do
        H.div ! A.id "filter-container" $
            H.form ! A.onsubmit "return false;" $
                H.input ! A.id "course-filter"
                        ! A.class_ "form-control"
                        ! A.placeholder "Enter a course!"
                        ! A.autocomplete "off"
                        ! A.type_ "text"
        H.div ! A.id "search-container" $
            H.div ! A.id "search-list" $ ""

infoPanel :: H.Html
infoPanel =
    H.div ! A.class_ "col-md-8 col-xs-12 col-md-pull-2" $
        H.div ! A.id "info" ! A.class_ "row" $ ""
