import Distribution.Simple
import System.Exit
import System.Process (system)
import Distribution.PackageDescription (emptyHookedBuildInfo)
import System.Directory (doesFileExist, copyFile)

main = defaultMainWithHooks
        simpleUserHooks { preBuild = preBuildChecks }
    where
        -- | Checks existence of Config.hs. If it doesn't exist, copy it from DevelopmentConfig.hs
        -- And check that Imagemagick and LaTeX are available
        preBuildChecks _ _ = do
            mapM_ checkDependency ["convert", "pdflatex"]
            configExistbool <- doesFileExist "app/Config.hs"
            case configExistbool of
                False -> copyFile "app/DevelopmentConfig.hs" "app/Config.hs" >> return emptyHookedBuildInfo
                _     -> return emptyHookedBuildInfo

        checkDependency :: String -> IO ()
        checkDependency dependency = do
            result <- system $ dependency ++ " -version"
            case result of
                ExitFailure 127 -> print ("Error Message: " ++ dependency ++ " is NOT available. Please add it in your path.") >> exitFailure
                _               -> print (dependency ++ " has been installed.")
