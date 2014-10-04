{-# LANGUAGE OverloadedStrings #-}

module MakeElements where
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

makeLink :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html
makeLink rel type_ href = H.link ! A.rel rel ! A.type_ type_ ! A.href href

makeScript :: H.AttributeValue -> H.Html
makeScript src = H.script ! A.src src $ ""

-- Headers
makeH1 :: H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeH1 id class_ content = H.h1 ! A.id id ! A.class_ class_ $ do content

makeH2 :: H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeH2 id class_ content = H.h2 ! A.id id ! A.class_ class_ $ do content

makeH3 :: H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeH3 id class_ content = H.h3 ! A.id id ! A.class_ class_ $ do content

makeH4 :: H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeH4 id class_ content = H.h4 ! A.id id ! A.class_ class_ $ do content

makeH5 :: H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeH5 id class_ content = H.h5 ! A.id id ! A.class_ class_ $ do content

makeP :: H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeP id class_ content = H.p ! A.id id ! A.class_ class_ $ do content

makeSpan :: H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeSpan id class_ content = H.span ! A.id id ! A.class_ class_ $ do content

makeDiv :: H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeDiv id class_ content = H.div ! A.id id ! A.class_ class_ $ do content

makeLi :: H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeLi id class_ content = H.li ! A.id id ! A.class_ class_ $ do content

makeUl :: H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeUl id class_ content = H.ul ! A.id id ! A.class_ class_ $ do content

makeForm :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeForm id class_ onSubmit content = H.form ! A.id id ! A.class_ class_ ! A.onsubmit onSubmit $ do content

makeInput :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html
makeInput id class_ placeholder autocomplete type_ = H.input ! A.id id ! A.class_ class_ ! A.placeholder placeholder ! A.autocomplete autocomplete ! A.type_ type_

makeA :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html
makeA id class_ href content = H.a ! A.id id ! A.class_ class_ ! A.href href $ do content
