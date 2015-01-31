{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module CourseographyFacebook where

import qualified Facebook as FB
import Control.Monad.IO.Class  (liftIO)
import qualified Data.Text as T
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

courseographyUrl :: String
courseographyUrl = "http://localhost:8000"

testUrl :: FB.RedirectUrl
testUrl = T.pack $ courseographyUrl ++ "/test"

testPostUrl :: FB.RedirectUrl
testPostUrl = T.pack $ courseographyUrl ++ "/test-post"

postFB :: String
postFB = "post-fb"

appId :: T.Text
appId = "442286309258193"

-- In order to access information as the Courseography application, the secret needs
-- to be declared in the third string below that has the place holder 'INSERT_SECRET'.
-- The secret should NEVER be committed to GitHub.
-- The secret can be found here: https://developers.facebook.com/apps/442286309258193/dashboard/
-- Should the secret be committed to GitHub, it needs to be reset immediately. If you find
-- yourself in this pickle, please contact someone who can do this.
appSecret :: T.Text
appSecret = "INSERT_SECRET"

credentials :: FB.Credentials
credentials = FB.Credentials (T.pack courseographyUrl) appId appSecret

perms :: [FB.Permission]
perms = []

-- | Constructs the Facebook authorization URL. This method does not actually
-- interact with Facebook.
retrieveAuthURL :: T.Text -> IO String
retrieveAuthURL url = 
	performFBAction $ do
        fbAuthUrl <- FB.getUserAccessTokenStep1 url perms
        return $ T.unpack fbAuthUrl

-- | The arguments passed to the API. The first argument is the key of the query data ('code')
-- and the second argument is the code that was retrieved in the first authorization step.
args :: String -> String -> FB.Argument
args arg1 arg2 = (BS.pack arg1, BS.pack arg2)

-- | Retrieves the user's email.
retrieveFBData :: String -> IO Response
retrieveFBData code = 
	performFBAction $ do
        token <- getToken testUrl code
        user <- FB.getUser "me" [] (Just token)
        liftIO $ insertIdIntoDb (FB.userId user)
        return $ toResponse (FB.userEmail user)

-- | Inserts a string into the database along with the current user's Facebook ID.
insertIdIntoDb :: FB.Id -> IO ()
insertIdIntoDb id_ = 
	runSqlite fbdbStr $ do
        runMigration migrateAll
        insert_ $ FacebookTest (show id_) "Test String"
        liftIO $ print "Inserted..."
        let sql = "SELECT * FROM facebook_test"
        rawQuery sql [] $$ CL.mapM_ (liftIO . print)

-- | Performs a Facebook action.
performFBAction :: FB.FacebookT FB.Auth (ResourceT IO) a -> IO a
performFBAction action = withManager $ \manager -> FB.runFacebookT credentials manager action

-- | Posts a message to the user's Facebook feed, with the code 'code'. GraphResponse
-- is then sent back to the user.
postToFacebook :: String -> ServerPart Response
postToFacebook code = (liftIO $ performPost code) >> graphResponse

-- | Performs the posting to facebook.
performPost :: String -> IO Response
performPost code = 
	performFBAction $ do
        postToFB code =<< getToken testPostUrl code
        return $ toResponse postFB

-- | Gets a user access token.
getToken :: (MonadResource m, MonadBaseControl IO m) => FB.RedirectUrl -> String -> FB.FacebookT FB.Auth m FB.UserAccessToken
getToken url code = FB.getUserAccessTokenStep2 url [args "code" code]

-- | Posts a message to Facebook.
postToFB :: (MonadResource m, MonadBaseControl IO m) => String -> FB.UserAccessToken -> FB.FacebookT FB.Auth m FB.Id
postToFB code token = FB.postObject "me/feed" [args "message" "Test Post Pls Ignore"] token

-- | Gets a user's Facebook email.
getEmail :: String -> ServerPart Response
getEmail code = liftIO $ retrieveFBData code