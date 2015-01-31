{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module CourseographyFacebook where

import qualified Facebook as FB
import Control.Monad.IO.Class  (liftIO)
import qualified Data.Text as T
import Happstack.Server
import Happstack.Server.Internal.Multipart
import Network.HTTP.Conduit (withManager, parseUrl, httpLbs, RequestBody(..))
import Control.Monad.Trans.Resource
import qualified Data.Conduit.List as CL
import Data.Conduit
import Data.Text.Encoding as TE
import Database.Persist
import ConvertSVGToPNG
import JsonParser
import System.Process
import GraphResponse
import GHC.IO.Exception
import Data.Maybe
import Tables
import Text.Digestive.Form as TEF
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Database.Persist.Sqlite
import Network.HTTP.Client.MultipartFormData

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
postToFacebook code = (liftIO $ performPost code) >> graphResponse

-- | Performs the posting to facebook.
performPost :: String -> IO Response
performPost code = do
	x <- BS.readFile "test.png"
	--let y = show $ BodyPart "source" x
	performFBAction $ do
        liftIO createPNGFile
        --postToFB (BS.unpack x) code =<< getToken url2 code
        liftIO $ removePNG "test.png"
        liftIO $ print "File Deleted."
        return $ toResponse postFB

createPNGFile :: IO ExitCode
createPNGFile =  do 
                   (inp,out,err,pid) <- convertSVGToPNG "test.png" "../res/graphs/graph_regions.svg"
                   liftIO $ waitForProcess pid

-- | Gets a user access token.
getToken :: (MonadResource m, MonadBaseControl IO m) => FB.RedirectUrl -> String -> FB.FacebookT FB.Auth m FB.UserAccessToken
getToken url code = FB.getUserAccessTokenStep2 url [args "code" code]

-- | Posts a message to Facebook.
postToFB :: (MonadResource m, MonadBaseControl IO m) => String -> String -> FB.UserAccessToken -> FB.FacebookT FB.Auth m FB.Id
postToFB dataString code token = FB.postObject "me/photos" [args "message" "Test Post Pls Ignore",
                                                            args "url" $ "teststr"] token

-- | Gets a users Facebook email.
getEmail :: String -> ServerPart Response
getEmail code = liftIO $ retrieveFBData code