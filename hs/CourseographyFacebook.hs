{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module CourseographyFacebook where

import qualified Facebook as FB
import Control.Monad.IO.Class  (liftIO)
import Happstack.Server
import Network.HTTP.Conduit (withManager)
import Control.Monad.Trans.Resource
import qualified Data.Conduit.List as CL
import Data.Conduit
import Database.Persist
import JsonParser
import GraphResponse
import Tables
import qualified Data.ByteString.Char8 as BS
import Database.Persist.Sqlite

postFB :: String
postFB = "post-fb"


-- In order to access information as the Courseography application, the secret needs
-- to be declared in the third string below that has the place holder 'INSERT_SECRET'.
-- The secret should NEVER be committed to GitHub.
-- The secret can be found here: https://developers.facebook.com/apps/442286309258193/dashboard/
-- Should the secret be committed to GitHub, it needs to be reset immediately. If you find
-- yourself in this pickle, please contact someone who can do this.
app :: FB.Credentials
app = FB.Credentials "localhost" "442286309258193" "INSERT_SECRET"

url :: FB.RedirectUrl
url = "http://localhost:8000/test"

url2 :: FB.RedirectUrl
url2 = "http://localhost:8000/test-post"

perms :: [FB.Permission]
perms = []

-- | The arguments passed to the API. The first argument is the key of the query data ('code')
-- and the second argument is the code that was retrieved in the first authorization step.
args :: String -> FB.Argument
args code = ("code", BS.pack code)

-- | Retrieves the user's email.
retrieveFBData :: String -> IO Response
retrieveFBData code = performFBAction $ do
        token <- getToken url code
        user <- FB.getUser "me" [] (Just token)
        liftIO $ insertIdIntoDb (FB.userId user)
        return $ toResponse (FB.userEmail user)

insertIdIntoDb :: FB.Id -> IO ()
insertIdIntoDb id_ = runSqlite fbdbStr $ do
                       runMigration migrateAll
                       insert_ $ FacebookTest (show id_) "Test String"
                       liftIO $ print "Inserted..."
                       let sql = "SELECT * FROM facebook_test"
                       rawQuery sql [] $$ CL.mapM_ (liftIO . print)

performFBAction :: FB.FacebookT FB.Auth (ResourceT IO) a -> IO a
performFBAction action = withManager $ \manager -> FB.runFacebookT app manager action

postToFacebook :: String -> ServerPart Response
postToFacebook code = (liftIO $ performPost code) >> graphResponse

args2 :: FB.Argument
args2 = ("message", "Test post please ignore")

performPost :: String -> IO Response
performPost code = performFBAction $ do
        postToFB code =<< getToken url2 code
        return $ toResponse postFB

-- | Gets a user access token.
getToken :: (MonadResource m, MonadBaseControl IO m) => FB.RedirectUrl -> String -> FB.FacebookT FB.Auth m FB.UserAccessToken
getToken url code = FB.getUserAccessTokenStep2 url [args code]

-- | Posts a message to Facebook.
postToFB :: (MonadResource m, MonadBaseControl IO m) => String -> FB.UserAccessToken -> FB.FacebookT FB.Auth m FB.Id
postToFB code token = FB.postObject "me/feed" [args2] token

-- | Gets a users Facebook email.
getEmail :: String -> ServerPart Response
getEmail code = liftIO $ retrieveFBData code

fbAuth1Url :: String
fbAuth1Url = "https://www.facebook.com/dialog/oauth?client_id=442286309258193&redirect_uri=http://localhost:8000/test&scope=user_birthday"

fbAuth1UrlPost :: String
fbAuth1UrlPost = "https://www.facebook.com/dialog/oauth?client_id=442286309258193&redirect_uri=http://localhost:8000/test-post"
