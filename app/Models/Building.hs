module Models.Building
    (buildingsCSV,
    parseBuildings,
    getBuildingsFromCSV,
    getBuilding) where

import Config (runDb)
import Control.Monad.IO.Class (liftIO)
import Data.CSV (csvFile)
import qualified Data.Text as T
import Database.Persist.Sqlite (Filter, SqlPersistM, deleteWhere, entityVal, insertMany_,
                                selectFirst, (==.))
import Database.Tables (Building (Building), EntityField (BuildingCode))
import Filesystem.Path.CurrentOS as Path (append, decodeString, encodeString)
import System.Directory (getCurrentDirectory)
import Text.ParserCombinators.Parsec (parseFromFile)
import Util.Helpers (safeHead)

buildingsCSV :: IO Prelude.FilePath
buildingsCSV = do
    curDir <- getCurrentDirectory
    return $ Path.encodeString $ Path.append (Path.decodeString curDir) $ Path.append (Path.decodeString "db") (Path.decodeString "building.csv")

parseBuildings :: IO ()
parseBuildings = do
    buildingInfo <- getBuildingsFromCSV =<< buildingsCSV
    runDb $ do
        liftIO $ putStrLn "Inserting buildings"
        deleteWhere ([] :: [Filter Building]) :: SqlPersistM ()
        insertMany_ buildingInfo :: SqlPersistM ()

-- | Extract building names, codes, addresses, postal codes, latitude and longitude from csv file
getBuildingsFromCSV :: String -> IO [Building]
getBuildingsFromCSV buildingCSVFile = do
    buildingCSVData <- parseFromFile csvFile buildingCSVFile
    case buildingCSVData of
        Left _ -> error "csv parse error"
        Right buildingData ->
            return $ map (\b -> Building (T.pack $ safeHead "" b)
                                        (T.pack (b !! 1))
                                        (T.pack (b !! 2))
                                        (T.pack (b !! 3))
                                        (read (b !! 4) :: Double)
                                        (read (b !! 5) :: Double)) $ drop 1 buildingData

-- | Given a building code, get the persistent Building associated with it
getBuilding :: Maybe T.Text -> SqlPersistM (Maybe Building)
getBuilding rm =
    case rm of
        Nothing -> return Nothing
        Just r -> do
            maybeEntityBuilding <- selectFirst [BuildingCode ==. T.take 2 r] []
            case maybeEntityBuilding of
                Nothing -> return Nothing
                Just entBuilding -> return $ Just (entityVal entBuilding)
