{-|
  Module      : Util.Blaze
  Description : Contains methods for setting various HTML attributes.
-}
module Util.Blaze
    (toStylesheet,
     toScript,
     toLink,
     mdToHTML) where

import qualified Data.Text as T
import Data.Text.Lazy (Text)
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Markdown (def, markdown)

-- |Sets the html attributes to the href of the style sheet.
toStylesheet :: T.Text -> H.Html
toStylesheet href = H.link ! A.rel "stylesheet"
                         ! A.type_ "text/css"
                         ! A.href (H.textValue href)

-- |Sets the script attributes.
toScript :: T.Text -> H.Html
toScript src = H.script ! A.src (H.textValue src) $ ""

-- |Creates a link by setting the href attribute.
toLink :: T.Text -> T.Text -> H.Html
toLink link content = H.a ! A.href (H.textValue link)
                          $ H.toHtml content

-- | mdToHTML takes in the contents of a file written in Mark Down and converts it to
-- blaze-HTML.
mdToHTML :: Text -> H.Html
mdToHTML = markdown def
