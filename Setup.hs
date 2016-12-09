import Distribution.Simple
import System.Exit
import System.Process (system)
import Distribution.PackageDescription (emptyHookedBuildInfo)
import System.Directory (doesFileExist, copyFile)

main = defaultMainWithHooks
        simpleUserHooks { postCopy = checkDependencies, preBuild = checkConfigExist }
    where
        -- | Checks existence of Config.hs. If it doesn't exist, copy it from DevelopmentConfig.hs
        checkConfigExist _ _ = do
            configExistbool <- doesFileExist "app/Config.hs"
            case configExistbool of
                False -> copyFile "app/DevelopmentConfig.hs" "app/Config.hs" >> return emptyHookedBuildInfo
                _     -> return emptyHookedBuildInfo

        -- | check that Imagemagick and LaTeX are available
        checkDependencies _ _ _ _ = do
            mapM_ check ["convert", "pdflatex"]
        check :: String -> IO ()
        check depdency = do
            result <- system depdency
            case result of
                ExitFailure 127 -> print $ depdency ++ " is NOT available."
                _               -> print $ depdency ++ " is available."
