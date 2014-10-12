{-# LANGUAGE OverloadedStrings #-}

module MakeElements where
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

insertSVG :: H.AttributeValue -> H.Html
insertSVG src = H.object ! A.data_ src ! A.type_ "image/svg+xml" $ do ""


createTag :: (H.Html -> H.Html) ->  H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
createTag tag id class_ content = tag ! A.id id ! A.class_ class_ $ do content

makeLink :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html
makeLink rel type_ href = H.link ! A.rel rel ! A.type_ type_ ! A.href href

makeScript :: H.AttributeValue -> H.Html
makeScript src = H.script ! A.src src $ ""

-- Headers
makeForm :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeForm id class_ onSubmit content = H.form ! A.id id ! A.class_ class_ ! A.onsubmit onSubmit $ do content

makeInput :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html
makeInput id class_ placeholder autocomplete type_ = H.input ! A.id id ! A.class_ class_ ! A.placeholder placeholder ! A.autocomplete autocomplete ! A.type_ type_

makeA :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeA id class_ href content = H.a ! A.id id ! A.class_ class_ ! A.href href $ do content
