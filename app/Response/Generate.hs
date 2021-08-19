module Response.Generate
    (generateResponse) where

import Happstack.Server
import MasterTemplate
import Scripts
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

generateResponse :: ServerPart Response
generateResponse =
    ok $ toResponse $
        masterTemplate "Courseography - Generate"
                    []
                    (do
                        header "generate-prerequisites"
                        generatePrerequisites
                    )
                    generateScripts

generatePrerequisites :: H.Html
generatePrerequisites =
    H.html $ do
        H.head $
            H.title "Generate Prerequisites!"
        H.div ! A.id "generateRoot" $ ""
