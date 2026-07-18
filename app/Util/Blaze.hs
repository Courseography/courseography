-- |
--     Module      : Util.Blaze
--     Description : Contains functions for setting various HTML attributes.
module Util.Blaze (
    toStylesheet,
    toScript,
    toLink,
    mdToHTML,
) where

import qualified Data.Text as T
import Data.Text.Lazy (Text)
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Markdown (def, markdown)

-- | Set the HTML attributes to the href of the style sheet.
toStylesheet :: T.Text -> H.Html
toStylesheet href =
    H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href (H.textValue href)

-- | Set the script attributes.
toScript :: T.Text -> H.Html
toScript src = H.script ! A.src (H.textValue src) $ ""

-- | Create a link by setting the href attribute.
toLink :: T.Text -> T.Text -> H.Html
toLink link content =
    H.a ! A.href (H.textValue link) $
        H.toHtml content

-- | Convert the contents of a markdown file to blaze-HTML.
mdToHTML :: Text -> H.Html
mdToHTML = markdown def
