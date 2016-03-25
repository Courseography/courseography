{-# LANGUAGE OverloadedStrings #-}

module MasterTemplate
    (masterTemplate, header, disclaimer) where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (stringValue)
import Config (enableFb)
import Util.Blaze
import Scripts (globalScripts)

masterTemplate :: String -> [H.Html] -> H.Html -> H.Html -> H.Html
masterTemplate title headers body scripts =
    H.html $ do
        H.head $ do
            H.meta ! A.httpEquiv "Content-Type"
                   ! A.content "text/html;charset=utf-8"
            H.title (H.toHtml title)
            H.link ! A.rel "icon" ! A.type_ "image/png"
                   ! A.href "static/res/ico/favicon.png"
            sequence_ headers
            mapM_ toStylesheet [
                "/static/js/common/bootstrap.min.3.1.1.js",
                "static/style/app.css"]
        H.body $ do
            body
            mapM_ toScript globalScripts
            scripts

-- Insert the header of the Grid and Graph. This contains the year of the timetable, and
-- a link back to the Graph.
header :: String -> H.Html
header page =
    H.nav ! A.class_ "row header" $ do
        H.img ! A.id "courseography-header" ! A.src "static/res/img/logo.png"
             ! H.customAttribute "context" (stringValue page)
        H.ul ! A.id "nav-links" $ do
            H.li $ toLink "graph" "Graph"
            H.li $ toLink "grid" "Grid"
            H.li $ toLink "timesearch" "Search"
            H.li $ toLink "draw" "Draw"
            H.li $ toLink "post" "Check My POSt!"
            H.li $ toLink "about" "About"
            if page `elem` ["graph", "grid"]
            then H.li $ H.a ! A.id "nav-export" $ "Export"
            else ""
        if enableFb && page `elem` ["graph", "grid"]
        then
            H.div ! A.id "nav-fb" $ do
                H.span ! A.id "nav-fb-post" $
                    H.a ! A.id "post-fb" $ "Post..."
                H.span ! A.class_ "fb-login-button"
                       ! H.customAttribute "data-max-rows" "1"
                       ! H.customAttribute "data-size" "xlarge"
                       ! H.customAttribute "data-show-faces" "false"
                       ! H.customAttribute "data-auto-logout-link" "false"
                       ! H.customAttribute "data-default-audience" "friends"
                       $ ""
        else
            ""

disclaimer :: H.Html
disclaimer =
    H.div ! A.id "disclaimerDiv" $ do
        "DISCLAIMER: Both the "
        H.a ! A.href "http://www.artsandscience.utoronto.ca/ofr/timetable/winter/csc.html"
            $ "Official Timetable"
        " and "
        H.a ! A.href "http://www.artsandscience.utoronto.ca/ofr/calendar/index.html"
            $ "Calendar"
        " take precedence over the information presented here. "
        "It's important that you double-check your course selection, "
        "prerequisites, and your program plans."
        "Some graph edges may represent a corequisite rather than a prerequisite."
