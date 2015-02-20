{-# LANGUAGE OverloadedStrings #-}

module GridResponse where
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MakeElements
import MasterTemplate
import Scripts

gridResponse :: ServerPart Response
gridResponse =
    ok $ toResponse $
        masterTemplate "Courseography - Grid"
            [H.meta ! A.name "keywords"
                    ! A.content "",
                    timetableLinks
            ]
            (do  header "grid"
                 conflictDialog
                 createTag H.div "" "row main" $ do
                      coursePanel
                      searchPanel
                      infoPanel
                 disclaimer
            )
            timetableScripts


conflictDialog :: H.Html
conflictDialog = createTag H.div "dialog" "" "Conflicting courses are difficult to manage. Make sure you understand the added responsibility of having two or more conflicting courses."

coursePanel :: H.Html
coursePanel = 
    createTag H.div "course-select-wrapper" "col-md-2 col-xs-6" $
        createTag H.ul "course-select" "trapScroll-enabled" $
            createTag H.li "clear-all" "" $
                createTag H.h3 "" "" "Clear All"

searchPanel :: H.Html
searchPanel = 
    createTag H.div "search-layout" "col-md-2 col-xs-6 col-md-push-8" $ do
        createTag H.div "filter-container" "" $
            makeForm "" "" "return false;" $
            makeInput "course-filter" "form-control" "Enter a course!" "off" "text"
        createTag H.div "search-container" "" $
            createTag H.div "search-list" "" ""

infoPanel :: H.Html
infoPanel = 
    createTag H.div "" "col-md-8 col-xs-12 col-md-pull-2" $
        createTag H.div "info" "row" ""
                            {-createTag H.div "info" "row" $ do
                                    createTag H.div "info-layout" "" $ do
                                            H.div $ do
                                                   createTag H.span "course-info-code" "" ""
                                                   createTag H.span "course-info-title" "" ""

                                            H.h4 $ do
                                                   createTag H.span "section-stats-section" "" ""
                                                   createTag H.span "section-stats-instructor" "" ""

                                            createTag H.p "section-stats-enrol" "" ""
                            -}
