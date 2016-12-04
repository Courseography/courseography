module Export.PdfGenerator
    (createPDF) where

import System.Process
import GHC.IO.Handle.Types
import Export.ImageConversion (removeImage)
import Data.List.Utils (replace)

-- | Opens a new process to create a PDF from a TEX (texName) and deletes
-- the tex file and extra files created by pdflatex
createPDF :: String -> IO ()
createPDF texName  = do
  (_, _, _, pid) <- convertTexToPDF texName 
  print "Waiting for a process..."
  _ <- waitForProcess pid
  let auxFile = replace ".tex" ".aux" texName
      logFile = replace ".tex" ".log" texName
  _ <- removeImage (auxFile ++ " " ++ logFile ++ " " ++ texName)
  print "Process Complete"

-- | Create a process to use the pdflatex program to create a PDF from a TEX 
-- file (texName). The process is run in nonstop mode and so it will not block 
-- if an error occurs. The resulting PDF will have the same filename as texName.
convertTexToPDF :: String -> IO
                            (Maybe Handle,
                             Maybe Handle,
                             Maybe Handle,
                             ProcessHandle)
convertTexToPDF texName = createProcess $ CreateProcess
                      (ShellCommand $ "pdflatex -interaction=nonstopmode " ++ texName) 
                      Nothing
                      Nothing
                      CreatePipe
                      CreatePipe
                      CreatePipe
                      False
                      False
                      False
