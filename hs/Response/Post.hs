{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Response.Post
    (postResponse) where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MasterTemplate
import Scripts

postResponse :: ServerPart Response
postResponse =
    ok $ toResponse $
        masterTemplate "Courseography - Check My POSt!"
                    []
                    (do
                        header "post"
                        checkPost
                    )
                    postScripts

checkPost :: H.Html
checkPost =
    H.html $ do
        H.head $
            H.title "Check My Post!"
        H.nav ! A.id "posts" $ H.ul $ do
            H.li ! A.id "specialist" $ do
                H.a ! A.href "" $ "Specialist"
                H.div ! A.id "spec_creds" $ "(0/12.0)"
            H.li ! A.id "major" $ do
                H.a ! A.href "" $ "Major"
                H.div ! A.id "maj_creds" $ "(0/8.0)"
            H.li ! A.id "minor" $ do
                H.a ! A.href "" $ "Minor"
                H.div ! A.id "min_creds" $ "(0/4.0)"
        H.div ! A.id "all_posts" $ ""
