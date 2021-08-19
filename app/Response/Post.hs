module Response.Post
    (postResponse) where

import Happstack.Server
import MasterTemplate
import Scripts
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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
        H.div ! A.id "all_posts" $ ""
