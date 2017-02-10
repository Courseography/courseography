{-# LANGUAGE OverloadedStrings #-}

module Util.Blaze
    (toStylesheet,
     toScript,
     toLink,
     mdToHTML) where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Markdown (markdown, def)
import Data.Text.Lazy (Text)
import qualified Data.Text as T

toStylesheet :: T.Text -> H.Html
toStylesheet href = H.link ! A.rel "stylesheet"
                         ! A.type_ "text/css"
                         ! A.href (H.textValue href)

toScript :: T.Text -> H.Html
toScript src = H.script ! A.src (H.textValue src) $ ""

toLink :: T.Text -> T.Text -> H.Html
toLink link content = H.a ! A.href (H.textValue link)
                          $ H.toHtml content

-- | mdToHTML takes in the contents of a file written in Mark Down and converts it to
-- blaze-HTML.
mdToHTML :: Text -> H.Html
mdToHTML = markdown def
