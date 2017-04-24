import Distribution.Simple
import System.Exit
import System.Process (readProcessWithExitCode)
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
            (result, _, _) <- readProcessWithExitCode dependency ["-version"] ""
            case result of
                ExitFailure 127 -> putStrLn ("Error Message: " ++ dependency ++ " is NOT available. Please ensure it is on your path.") >> exitFailure
                _               -> putStrLn (dependency ++ " has been installed.")
