{-# LANGUAGE OverloadedStrings #-}

module Response.Privacy
    (privacyResponse) where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Text.Lazy (Text)
import Happstack.Server
import MasterTemplate
import Util.Blaze (mdToHTML)

privacyResponse :: Text -> ServerPart Response
privacyResponse privacyContents =
   ok $ toResponse $
    masterTemplate "Courseography - Privacy Policy"
                []
                (do
                    header "privacy"
                    privacyHtml privacyContents
                )
                ""

-- | privacyHtml takes in the contents of the PRIVACY.md file (the GitHub README file) and translates
--   the markdown to blaze-HTML.
privacyHtml :: Text -> H.Html
privacyHtml contents = H.div ! A.id "privacyDiv" $ mdToHTML contents
