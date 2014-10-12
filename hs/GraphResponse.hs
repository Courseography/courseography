{-# LANGUAGE OverloadedStrings #-}

module GraphResponse where
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MakeElements
import MasterTemplate

graphResponse :: ServerPart Response
graphResponse =
   ok $ toResponse $
    masterTemplate "Courseography - Svg serving test!"
                [H.meta ! A.name "keywords"
                        ! A.content "",
                 insertTimetableLinks
                ]
                (do  insertSVG "static/graph_regions.svg"

                     createTag H.div "" "row main" $ do
                           insertSVG "static/graph_regions.svg"
                )
                insertjQuery