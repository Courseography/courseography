{-|
    Module      : Export.PdfGenerator
    Description : Contains functions for generating a PDF from LaTeX text.
-}
module Export.PdfGenerator
    (createPDF) where

import Data.List.Utils (replace)
import Export.ImageConversion (removeFile)
import GHC.IO.Handle.Types
import System.Process (ProcessHandle, createProcess, shell, waitForProcess)

-- | Opens a new process to create a PDF from a TEX (texName) and deletes
-- the tex file and extra files created by pdflatex
createPDF :: String -> IO ()
createPDF texName  = do
  (_, _, _, pid) <- convertTexToPDF texName
  putStrLn "Waiting for a process..."
  _ <- waitForProcess pid
  let auxFile = replace ".tex" ".aux" texName
      logFile = replace ".tex" ".log" texName
  _ <- removeFile (auxFile ++ " " ++ logFile ++ " " ++ texName)
  putStrLn "Process Complete"

-- | Create a process to use the pdflatex program to create a PDF from a TEX
-- file (texName). The process is run in nonstop mode and so it will not block
-- if an error occurs. The resulting PDF will have the same filename as texName.
convertTexToPDF :: String -> IO
                            (Maybe Handle,
                             Maybe Handle,
                             Maybe Handle,
                             ProcessHandle)
convertTexToPDF texName =
    createProcess $ shell $ "pdflatex -interaction=nonstopmode " ++ texName
