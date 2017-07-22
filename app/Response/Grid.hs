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
                H.div ! A.id "grid-body"! A.class_ "row main" $ do
                     coursePanel
                     searchPanel
                     infoPanel
                disclaimer
            )
            timetableScripts

coursePanel :: H.Html
coursePanel =
    H.div ! A.id "course-select-wrapper" ! A.class_ "col-md-2 col-xs-6" $ ""

searchPanel :: H.Html
searchPanel =
    H.div ! A.id "search-layout" ! A.class_ "col-md-2 col-xs-6 col-md-push-8" $
        ""

infoPanel :: H.Html
infoPanel =
    H.div ! A.class_ "col-md-8 col-xs-12 col-md-pull-2" $
        H.div ! A.id "info" ! A.class_ "row" $ ""
