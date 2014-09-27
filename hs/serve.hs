{-# LANGUAGE OverloadedStrings #-}
module Main where

import Happstack.Server
import           Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

appTemplate :: String -> [H.Html] -> H.Html -> H.Html
appTemplate title headers body =
    H.html $ do
      H.head $ do
        H.title (H.toHtml title)
        H.meta ! A.httpEquiv "Content-Type"
               ! A.content "text/html;charset=utf-8"
        sequence_ headers
      H.body $ do
        body

helloBlaze :: ServerPart Response
helloBlaze =
   ok $ toResponse $
    appTemplate "Hello, David Liu!"
                [H.meta ! A.name "keywords"
                        ! A.content "happstack, david, liu"
                ]
                (H.p $ do "Hello, "
                          H.b "David Liu!!")

main :: IO ()
main = simpleHTTP nullConf $ helloBlaze