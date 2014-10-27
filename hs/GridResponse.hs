{-# LANGUAGE OverloadedStrings #-}

module GridResponse where
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MakeElements
import MasterTemplate

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
                )
                timetableScripts

-- Create <scripts/>
timetableScripts :: H.Html
timetableScripts = do jQuery
                      concatHtml (map makeScript ["static/js/grid/timetable_util.js",
                                                "static/js/grid/setup.js",
                                                "static/js/grid/mouse_events.js",
                                                "//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js",
                                                "http://code.jquery.com/ui/1.10.4/jquery-ui.js",
                                                "static/js/common/cookieHandler.js",
                                                "static/js/grid/generate_grid.js",
                                                "static/js/common/objects/course.js",
                                                "static/js/common/objects/section.js",
                                                "static/js/common/utilities/util.js"])


conflictDialog :: H.Html
conflictDialog = createTag H.div "dialog" "" "Conflicting courses are difficult to manage. Make sure you understand the added responsibility of having two or more conflicting courses."

coursePanel :: H.Html
coursePanel = createTag H.div "course-select-wrapper" "col-md-2 col-xs-6" $ do
                            createTag H.ul "course-select" "trapScroll-enabled" $ do
                                   createTag H.li "clear-all" "" $ do
                                          createTag H.h3 "" "" "Clear All"

searchPanel :: H.Html
searchPanel =  createTag H.div "search-layout" "col-md-2 col-xs-6 col-md-push-8" $ do
                             createTag H.div "filter-container" "" $ do
                                     makeForm "" "" "return false;" $ do
                                     makeInput "course-filter" "form-control" "Enter a course!" "off" "text"
                             createTag H.div "search-container" "" $ do
                                     createTag H.div "search-list" "" ""

infoPanel :: H.Html
infoPanel = createTag H.div "" "col-md-8 col-xs-12 col-md-pull-2" $ do
                            createTag H.div "info" "row" $ do
                                    createTag H.div "info-layout" "" $ do
                                            createTag H.div "" "" $ do
                                                   createTag H.span "course-info-code" "" ""
                                                   createTag H.span "course-info-title" "" ""

                                            createTag H.h4 "" "" $ do
                                                   createTag H.span "section-stats-section" "" ""
                                                   createTag H.span "section-stats-instructor" "" ""

                                            createTag H.p "section-stats-enrol" "" ""
