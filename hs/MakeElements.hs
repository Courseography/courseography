{-# LANGUAGE OverloadedStrings #-}

module MakeElements where
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Monad    (msum)

insertSVG :: H.AttributeValue -> H.Html
insertSVG src = H.object ! A.data_ src ! A.type_ "image/svg+xml" $ ""

createTag :: (H.Html -> H.Html) ->  H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
createTag tag id class_ content = tag ! A.id id ! A.class_ class_ $ content

makeLink :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html
makeLink rel type_ href = H.link ! A.rel rel ! A.type_ type_ ! A.href href

stylesheet :: H.AttributeValue -> H.Html
stylesheet href = H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href href

makeScript :: H.AttributeValue -> H.Html
makeScript src = H.script ! A.src src $ ""

makeForm :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeForm id class_ onSubmit content = H.form ! A.id id ! A.class_ class_ ! A.onsubmit onSubmit $ content

makeInput :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html
makeInput id class_ placeholder autocomplete type_ = H.input ! A.id id ! A.class_ class_ ! A.placeholder placeholder ! A.autocomplete autocomplete ! A.type_ type_

makeA :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeA id class_ href target content = H.a ! A.id id ! A.class_ class_ ! A.href href ! A.target target $ content

tabAnchor :: H.AttributeValue -> H.Html -> H.Html
tabAnchor href content = H.a ! A.href href $ content

jQuery :: H.Html
jQuery = makeScript "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"

timetableLinks :: H.Html
timetableLinks = concatHtml (map stylesheet ["//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css",
                                         "static/style/grid/timetable_styles.css",
                                         "static/style/common/common.css"])

plannerLinks :: H.Html
plannerLinks = concatHtml (map stylesheet ["//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css",
                                       "static/style/graph/graph_styles.css",
                                       "static/res/video-js/video-js.css",
                                       "static/style/common/common.css"])

aboutLinks :: H.Html
aboutLinks = concatHtml (map stylesheet ["//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css",
                                       "static/style/common/about.css",
                                       "static/style/common/common.css"])

concatHtml :: [H.Html] -> H.Html
concatHtml html = sequence_ html