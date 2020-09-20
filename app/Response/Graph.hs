module Response.Graph
    (graphResponse) where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MasterTemplate
import Scripts

graphResponse :: ServerPart Response
graphResponse =
   ok $ toResponse $
    masterTemplate "Courseography - Graph"
                []
                (do
                    header "graph"
                    H.div ! A.id "container" $ do
                        -- TODO: remove this inner div
                        H.div ! A.id "react-graph" ! A.class_ "react-graph" $ ""
                    disclaimer
                )
                graphScripts
