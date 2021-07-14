module MasterTemplate
    (masterTemplate, header) where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T
import Text.Blaze.Internal (textValue)
import Config (enableCdn)
import Util.Blaze
import Scripts (globalScripts)

masterTemplate :: T.Text -> [H.Html] -> H.Html -> H.Html -> H.Html
masterTemplate title headers body scripts =
    H.html $ do
        H.head $ do
            H.meta ! A.httpEquiv "Content-Type"
                   ! A.content "text/html;charset=utf-8"
            H.title (H.toHtml title)
            H.link ! A.rel "icon" ! A.type_ "image/png"
                   ! A.href "/static/res/ico/favicon.png"
            H.link ! A.rel "stylesheet" ! A.href "https://unpkg.com/leaflet@1.5.1/dist/leaflet.css"
            H.script ! A.src "https://unpkg.com/leaflet@1.5.1/dist/leaflet.js"
                     $ H.toHtml ("" :: String)
            sequence_ headers
            mapM_ toStylesheet [
                if enableCdn
                then "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"
                else "/static/style/bootstrap.min.3.1.1.css",
                "/static/style/app.css"]
        H.body $ do
            body
            mapM_ toScript globalScripts
            scripts

-- Insert the header of the Grid and Graph. This contains the year of the timetable, and
-- a link back to the Graph.
header :: T.Text -> H.Html
header page =
    H.nav ! A.class_ "row header" $ do
        H.div ! A.id "logo-div" $ do
            H.img ! A.id "courseography-header" ! A.src "/static/res/img/logo.png"
                ! H.customAttribute "context" (textValue page)
        H.div ! A.id "nav-bar" $ do
            H.ul ! A.id "nav-links" $ do
                H.li $ toLink "/graph" "Graph"
                H.li $ toLink "/grid" "Grid"
                H.li $ toLink "/generate" "Generate (beta)"
                -- H.li $ toLink "/timesearch" "Search"
                -- H.li $ toLink "/draw" "Draw"
                -- TODO: re-enable after handling new first-year courses
                -- H.li $ toLink "post" "Check My POSt!"
                H.li $ toLink "/about" "About"
        if page `elem` ["graph", "grid"]
        then H.div ! A.id "nav-export" $ do
            H.button ! A.id "nav-export-button" $ do
                H.img ! A.src "/static/res/ico/export.png" ! A.alt "Export"
        else ""
