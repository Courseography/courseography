module MasterTemplate
    (masterTemplate, header) where

import Config (enableCdn)
import qualified Data.Text as T
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (textValue)
import Util.Blaze


masterTemplate :: T.Text -> [H.Html] -> H.Html -> H.Html -> H.Html
masterTemplate title headers body scripts =
    H.docTypeHtml $ do
        H.head $ do
            H.meta ! A.httpEquiv "Content-Type"
                   ! A.content "text/html;charset=utf-8"
            H.title (H.toHtml title)
            H.link ! A.rel "icon" ! A.type_ "image/png"
                   ! A.href "/static/res/ico/favicon.png"
            H.link ! A.rel "stylesheet" ! A.href "https://unpkg.com/leaflet@1.5.1/dist/leaflet.css"
            sequence_ headers
            mapM_ toStylesheet [
                if enableCdn
                then "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"
                else "/static/style/bootstrap.min.3.1.1.css",
                "/static/style/app.css"]
        H.body $ do
            body
            scripts

-- Insert the header of the Grid and Graph. This contains the year of the timetable, and
-- a link back to the Graph.
header :: T.Text -> H.Html
header page =
    H.nav ! A.class_ "row header" $ do
        H.div ! A.class_ "nav-left" $ do
            H.a ! A.href "/graph" $ do
                H.img ! A.id "courseography-header" ! A.src "/static/res/img/logo.png"
                    ! H.customAttribute "context" (textValue page)
        H.div ! A.class_ "nav-middle" $ do
            H.ul ! A.id "nav-links" $ do
                if page == "graph"
                    then H.li ! A.id "nav-graph" ! A.class_ "selected-page" $ toLink "/graph" "Graph"
                else H.li ! A.id "nav-graph" $ toLink "/graph" "Graph"
                if page == "grid"
                    then H.li ! A.class_ "selected-page" $ toLink "/grid"  "Grid"
                else H.li $ toLink "/grid" "Grid"
                if page == "generate-prerequisites"
                    then H.li ! A.class_ "selected-page" $ toLink "/generate" "Generate (beta)"
                else H.li ! A.id "nav-generate" $ toLink "/generate" "Generate (beta)"
                -- H.li $ toLink "/timesearch" "Search"
                -- H.li $ toLink "/draw" "Draw"
                -- TODO: re-enable after handling new first-year courses
                -- H.li $ toLink "post" "Check My POSt!"
                if page == "about"
                    then H.li ! A.class_ "selected-page" $ toLink "/about" "About"
                else H.li $ toLink "/about" "About"
        H.div ! A.class_ "nav-right" $ do
            if page `elem` ["graph", "grid"]
            then H.button ! A.id "nav-export" $ do
                H.img ! A.src "/static/res/ico/export.png" ! A.alt "Export"
            else ""
