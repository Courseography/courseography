module ConvertSVGToPNG where

import System.Process
import GHC.IO.Handle.Types

convertSVGToPNG :: String -> String -> IO
                     (Maybe Handle,
                      Maybe Handle,
                      Maybe Handle,
                      ProcessHandle)

convertSVGToPNG inName outName = createProcess $ CreateProcess
                                  (ShellCommand $ "inkscape -z -e " ++
                                   inName ++
                                   " -w 1024 -h 1024 " ++
                                   outName)
                                  Nothing
                                  Nothing
                                  Inherit
                                  Inherit
                                  Inherit
                                  False
                                  False
removePNG :: String -> IO
                     (Maybe Handle,
                      Maybe Handle,
                      Maybe Handle,
                      ProcessHandle)

removePNG name = createProcess $ CreateProcess
                                  (ShellCommand $ "rm " ++ name)
                                  Nothing
                                  Nothing
                                  Inherit
                                  Inherit
                                  Inherit
                                  False
                                  False