module Response.Graph
    (graphResponse) where

import Happstack.Server
import MasterTemplate
import Scripts
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

graphResponse :: ServerPart Response
graphResponse =
   ok $ toResponse $
    masterTemplate "Courseography - Graph"
                []
                (do
                    header "graph"
                    H.div ! A.id "container" $ ""
                )
                graphScripts
