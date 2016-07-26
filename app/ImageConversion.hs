module ImageConversion
    (getGraphImage, getTimetableImage, createPDF, createImageFile, removeImage) where

import System.Process
import GHC.IO.Handle.Types
import TimetableImageCreator (renderTable)
import qualified Data.Map as M
import System.Random
import Svg.Generator
import Database.Tables (GraphId)

-- | Creates an image, and returns the name of the svg used to create the
-- image and the name of the image
getGraphImage :: GraphId -> M.Map String String -> IO (String, String)
getGraphImage gId courseMap = do
    gen <- newStdGen
    let (rand, _) = next gen
        svgFilename = show rand ++ ".svg"
        imageFilename = show rand ++ ".png"
    buildSVG gId courseMap svgFilename True
    createImageFile svgFilename imageFilename
    return (svgFilename, imageFilename)

-- | Creates an image, and returns the name of the svg used to create the
-- image and the name of the image
getTimetableImage :: String -> String -> IO (String, String)
getTimetableImage courses session = do
    gen <- newStdGen
    let (rand, _) = next gen
        svgFilename = show rand ++ ".svg"
        imageFilename = show rand ++ ".png"
    renderTable svgFilename courses session
    createImageFile svgFilename imageFilename
    return (svgFilename, imageFilename)

-- | Opens a new process to create a .pdf file from a .tex file and waits for that process to terminate.
createPDF :: String -> String -> IO ()
createPDF texName imgName = do
  -- Get the pid of process that is opened for converting .tex to .pdf
  (_, _, _, pid) <- convertTexToPDF texName imgName
  print "Waiting for a process..."
  -- Force current process to wait for conversion to finish
  waitForProcess pid
  print "Process Complete"

-- | Create a process to use the pdflatex program to create a .pdf file from a .tex file using a supplied image
convertTexToPDF :: String -> String -> IO
                                   (Maybe Handle,
                                    Maybe Handle,
                                    Maybe Handle,
                                    ProcessHandle)

convertTexToPDF texName imgName = createProcess $ CreateProcess
                      (ShellCommand $ "pdflatex " ++
                              "--jobname=" ++
                              imgName ++ " " ++
                              "\'\\def\\img{" ++
                              imgName ++
                              "}\\input \'" ++
                              texName
                      ) -- runs pdflatex from shell command
                      Nothing
                      Nothing
                      CreatePipe
                      CreatePipe
                      CreatePipe
                      False
                      False
                      False

-- Note: hGetContents can be used to read Handles. Useful when trying to read from
-- stdout.
createImageFile :: String -> String -> IO ()
createImageFile inName outName =  do
    (_, _, _, pid) <- convertToImage inName outName
    print "Waiting for process..."
    waitForProcess pid
    print "Process Complete"

-- | Converts an SVG file to a PNG file. Note that image magik's 'convert' command
-- can take in file descriptors.
convertToImage :: String -> String -> IO
                     (Maybe Handle,
                      Maybe Handle,
                      Maybe Handle,
                      ProcessHandle)

convertToImage inName outName = createProcess $ CreateProcess
                                  (ShellCommand $ "convert " ++
                                                  inName ++
                                                  " " ++
                                                  outName
                                  )
                                  Nothing
                                  Nothing
                                  CreatePipe
                                  CreatePipe
                                  CreatePipe
                                  False
                                  False
                                  False
-- | Removes a file.
removeImage :: String -> IO
                     (Maybe Handle,
                      Maybe Handle,
                      Maybe Handle,
                      ProcessHandle)

removeImage name = createProcess $ CreateProcess
                                  (ShellCommand $ "rm " ++ name)
                                  Nothing
                                  Nothing
                                  Inherit
                                  CreatePipe
                                  CreatePipe
                                  False
                                  False
                                  False