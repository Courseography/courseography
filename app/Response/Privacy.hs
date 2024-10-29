module Response.Privacy
    (privacyResponse) where

import Happstack.Server
import MasterTemplate
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Scripts (privacyScripts)

privacyResponse :: ServerPart Response
privacyResponse =
   ok $ toResponse $
    masterTemplate "Courseography - Privacy Policy"
                []
                (do
                    header "privacy"
                    H.div ! A.id "privacyDiv" $ ""
                )
                privacyScripts
