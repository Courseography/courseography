{-# LANGUAGE OverloadedStrings #-}

module MakeElements where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Svg11 as S

insertSVG :: H.AttributeValue -> H.Html
insertSVG src = H.object ! A.data_ src ! A.type_ "image/svg+xml" $ ""

createTag :: (H.Html -> H.Html) ->  H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
createTag tag identity class_ content = tag ! A.id identity ! A.class_ class_ $ content

stylesheet :: H.AttributeValue -> H.Html
stylesheet href = H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href href

makeScript :: H.AttributeValue -> H.Html
makeScript src = H.script ! A.src src $ ""

makeForm :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeForm identity class_ onSubmit content = H.form ! A.id identity ! A.class_ class_ ! A.onsubmit onSubmit $ content

makeInput :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html
makeInput identity class_ placeholder autocomplete type_ = H.input ! A.id identity ! A.class_ class_ ! A.placeholder placeholder ! A.autocomplete autocomplete ! A.type_ type_

makeA :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeA identity class_ href target content = H.a ! A.id identity ! A.class_ class_ ! A.href href ! A.target target $ content

jQuery :: H.Html
jQuery = makeScript "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"

concatHtml :: [H.Html] -> H.Html
concatHtml html = sequence_ html

concatSVG :: [S.Svg] -> S.Svg
concatSVG svg = sequence_ svg
