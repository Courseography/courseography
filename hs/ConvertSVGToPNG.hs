module ConvertSVGToPNG where

import System.Process
import GHC.IO.Handle.Types

convertSVGToPNG :: IO
                     (Maybe Handle,
                      Maybe Handle,
                      Maybe Handle,
                      ProcessHandle)

convertSVGToPNG = createProcess $ CreateProcess
                                  (ShellCommand "inkscape -z -e test.png -w 1024 -h 1024 ../res/graphs/graph_regions.svg")
                                  Nothing
                                  Nothing
                                  Inherit
                                  Inherit
                                  Inherit
                                  False
                                  False
