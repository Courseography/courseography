{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs #-}
module CourseographyFacebook where

import qualified Facebook as FB
import Control.Monad.IO.Class  (liftIO)
import qualified Data.Text as T
import Happstack.Server
import Happstack.Server.Internal.Multipart
import Network.HTTP.Conduit (withManager, parseUrl, httpLbs)
import Control.Monad.Trans.Resource
import qualified Data.Conduit.List as CL
import Data.Conduit
import Database.Persist
import ConvertSVGToPNG
import JsonParser
import System.Process
import GraphResponse
import GHC.IO.Exception
import Tables
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Database.Persist.Sqlite
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Client (RequestBody(..))
import Network (withSocketsDo)


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

url1 :: FB.RedirectUrl
url1 = "http://localhost:8000/test"

url2 :: FB.RedirectUrl
url2 = "http://localhost:8000/test-post"

perms :: [FB.Permission]
perms = ["publish_actions"]

postPhoto :: FB.UserAccessToken -> IO Response
postPhoto (FB.UserAccessToken _ b _) = withSocketsDo $ withManager $ \m -> do
    fbpost <- parseUrl $ "https://graph.facebook.com/v2.2/me/photos?access_token=" ++ T.unpack b
    flip httpLbs m =<<
        (formDataBody [partBS "message" "Test Message",
                       partFileSource "graph" "graph.png"]
                      fbpost)
    return $ toResponse postFB

-- | Constructs the Facebook authorization URL. This method does not actually
-- interact with Facebook.
retrieveAuthURL :: T.Text -> IO String
retrieveAuthURL url = 
    performFBAction $ do
        fbAuthUrl <- FB.getUserAccessTokenStep1 url perms
        return $ T.unpack fbAuthUrl

-- | Retrieves the user's email.
retrieveFBData :: BS.ByteString -> IO Response
retrieveFBData code = 
    performFBAction $ do
        token <- getToken url1 code
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
performFBAction action = withManager $ \manager -> FB.runFacebookT app manager action

-- | Posts a message to the user's Facebook feed, with the code 'code'. GraphResponse
-- is then sent back to the user.
postToFacebook :: String -> ServerPart Response
postToFacebook code = (liftIO $ performPost (BS.pack code)) >> graphResponse

-- | Performs the posting to facebook.
performPost :: BS.ByteString -> IO Response
performPost code = do
    createPNGFile
    performFBAction $ do
        token <- getToken url2 code
        liftIO $ postPhoto token
        return $ toResponse postFB

createPNGFile :: IO ExitCode
createPNGFile =  do (inp,out,err,pid) <- convertSVGToPNG "graph.png" "../res/graphs/graph_regions.svg"
                    liftIO $ waitForProcess pid

-- | Gets a user access token.
getToken :: (MonadResource m, MonadBaseControl IO m) => FB.RedirectUrl -> BS.ByteString -> FB.FacebookT FB.Auth m FB.UserAccessToken
getToken url code = FB.getUserAccessTokenStep2 url [("code", code)]

-- | Gets a users Facebook email.
getEmail :: String -> ServerPart Response
getEmail code = liftIO $ retrieveFBData (BS.pack code)