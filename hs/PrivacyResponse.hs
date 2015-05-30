{-# LANGUAGE OverloadedStrings #-}

module PrivacyResponse where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Text.Lazy (Text)
import Text.Markdown (markdown, def)
import Happstack.Server
import MasterTemplate

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

-- | mdToHTML takes in the contents of a file written in Mark Down and converts it to
-- blaze-HTML.
mdToHTML :: Text -> H.Html
mdToHTML contents = markdown def contents

