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
                H.div ! A.id "grid-body"! A.class_ "row main" $ ""
                disclaimer
            )
            timetableScripts
