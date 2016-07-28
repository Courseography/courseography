module PdfGenerator
    (createPDF) where

import System.Process
import GHC.IO.Handle.Types

-- | Opens a new process to create a .pdf file (with the same name as imgName) 
-- from a .tex file (texName) that contains the image imgName and waits for
-- that process to terminate.
createPDF :: String -> String -> IO ()
createPDF texName imgName = do
  -- Get the pid of process that is opened for converting .tex to .pdf
  (_, _, _, pid) <- convertTexToPDF texName imgName
  print "Waiting for a process..."
  -- Force current process to wait for conversion to finish
  waitForProcess pid
  print "Process Complete"

-- | Create a process to use the pdflatex program to create a .pdf file (named 
-- imgName) from a .tex file (texName) using a supplied image (imgName)
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