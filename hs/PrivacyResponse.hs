{-# LANGUAGE OverloadedStrings #-}

module PrivacyResponse where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Text.Lazy (Text)
import Text.Markdown (markdown, def)
import Happstack.Server
import MakeElements
import MasterTemplate

privacyResponse :: Text -> ServerPart Response
privacyResponse privacyContents =
   ok $ toResponse $
    masterTemplate "Courseography - Privacy Policy"
                [H.meta ! A.name "keywords"
                        ! A.content "",
                 aboutLinks
                ]
                (do
                    header "privacy"
                    privacyHtml privacyContents
                )
                ""

-- | AboutHtml takes in the contents of the README.md file (the GitHub README file) and translates
--   the markdown to blaze-HTML.
privacyHtml :: Text -> H.Html
privacyHtml contents = H.div $ mdToHTML contents

-- | mdToHTML takes in the contents of a file written in Mark Down and converts it to 
-- blaze-HTML.
mdToHTML :: Text -> H.Html
mdToHTML contents = markdown def contents

