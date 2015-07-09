{-# LANGUAGE OverloadedStrings #-}

module Utilities
    (createTag,
     stylesheet,
     makeScript,
     makeForm,
     makeInput,
     makeA,
     jQuery,
     concatHtml,
     concatSVG,
     mdToHTML) where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Svg11 as S
import Text.Markdown (markdown, def)
import Data.Text.Lazy (Text)

createTag :: (H.Html -> H.Html) ->  H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
createTag tag id_ class_ content = tag ! A.id id_ ! A.class_ class_ $ content

stylesheet :: H.AttributeValue -> H.Html
stylesheet href = H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href href

makeScript :: H.AttributeValue -> H.Html
makeScript src = H.script ! A.src src $ ""

makeForm :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeForm id_ class_ onSubmit content = H.form ! A.id id_ ! A.class_ class_ ! A.onsubmit onSubmit $ content

makeInput :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html
makeInput id_ class_ placeholder autocomplete type_ = H.input ! A.id id_ ! A.class_ class_ ! A.placeholder placeholder ! A.autocomplete autocomplete ! A.type_ type_

makeA :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeA id_ class_ href target content = H.a ! A.id id_ ! A.class_ class_ ! A.href href ! A.target target $ content

jQuery :: H.Html
jQuery = makeScript "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"

concatHtml :: [H.Html] -> H.Html
concatHtml html = sequence_ html

concatSVG :: [S.Svg] -> S.Svg
concatSVG svg = sequence_ svg

-- | mdToHTML takes in the contents of a file written in Mark Down and converts it to
-- blaze-HTML.
mdToHTML :: Text -> H.Html
mdToHTML contents = markdown def contents

-- | The last function is a generic helper function for running an arbitrary
-- query to the database. This should only be used for debugging purposes,
-- and in fact probably moved elsewhere.

-- | Performs a query on the database.
queryDatabase :: TI.Text -> IO ()
queryDatabase sql = runSqlite databasePath $
                        rawQuery sql [] $$ CL.mapM_ (liftIO . print)
