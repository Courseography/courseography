{-# LANGUAGE OverloadedStrings #-}

module MakeElements where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Svg11 as S
import Control.Monad    (msum)

insertSVG :: H.AttributeValue -> H.Html
insertSVG src = H.object ! A.data_ src ! A.type_ "image/svg+xml" $ ""

createTag :: (H.Html -> H.Html) ->  H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
createTag tag id class_ content = tag ! A.id id ! A.class_ class_ $ content

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

jQuery :: H.Html
jQuery = makeScript "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"

timetableLinks :: H.Html
timetableLinks = concatHtml (map stylesheet ["//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css",
                                         "static/style/grid/timetable_styles.css",
                                         "static/style/common/common.css"])

plannerLinks :: H.Html
plannerLinks = concatHtml (map stylesheet ["//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css",
                                       "static/style/graph/graph_styles.css",
                                       "//vjs.zencdn.net/4.12/video-js.css",
                                       "static/style/common/common.css"])

aboutLinks :: H.Html
aboutLinks = concatHtml (map stylesheet ["//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css",
                                       "static/style/common/about.css",
                                       "static/style/common/common.css"])
drawLinks :: H.Html
drawLinks = concatHtml (map stylesheet ["//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css",
                                       "static/style/graph/graph_styles.css",
                                       "static/style/common/common.css",
                                       "static/style/draw/draw_styles.css"])

postLinks :: H.Html
postLinks = concatHtml (map stylesheet ["//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css",
                                        "static/style/post/post_styles.css",
                                        "static/style/common/common.css"])

fourOhFourLinks :: H.Html
fourOhFourLinks = concatHtml (map stylesheet ["//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css",
                                              "static/style/common/common.css",
                                              "static/style/common/four_oh_four.css"])

searchLinks :: H.Html
searchLinks = concatHtml (map stylesheet ["//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css",
                                          "static/style/common/common.css",
                                          "static/style/search/search_styles.css"])

concatHtml :: [H.Html] -> H.Html
concatHtml html = sequence_ html


concatSVG :: [S.Svg] -> S.Svg
concatSVG svg = sequence_ svg
