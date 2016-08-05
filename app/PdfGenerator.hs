module PdfGenerator
    (createPDF) where

import System.Process
import GHC.IO.Handle.Types
import ImageConversion (removeImage)

-- | Opens a new process to create a .pdf file (pdfName) from a .tex 
-- file (texName) that contains graphImg and timetableImg and deletes
-- extra files created by pdflatex
createPDF :: String -> String -> String -> String -> IO ()
createPDF texName pdfName graphImg timetableImg = do
  (_, _, _, pid) <- convertTexToPDF texName pdfName graphImg timetableImg
  print "Waiting for a process..."
  waitForProcess pid
  removeImage (pdfName ++ ".aux") -- delete log and aux files created in the pdflatex process
  removeImage (pdfName ++ ".log")
  print "Process Complete"

-- | Create a process to use the pdflatex program to create a .pdf 
-- file (pdfName, which should not have file extension) from a .tex file 
-- (texName, which doesn't require file extension) that contains the supplied 
-- images graphImg timetableImg
convertTexToPDF :: String -> String -> String -> String -> IO
                                                          (Maybe Handle,
                                                           Maybe Handle,
                                                           Maybe Handle,
                                                           ProcessHandle)
convertTexToPDF texName pdfName graphImg timetableImg = createProcess $ CreateProcess
                      (ShellCommand $ "pdflatex -interaction=nonstopmode " ++   
                                      "--jobname=" ++ pdfName ++                -- declare name of resulting pdf
                                      " \"\\def\\graph{" ++ graphImg ++         -- provide graph image variable
                                      "}\\def\\timetable{" ++ timetableImg ++   -- provide timetable image variable
                                      "}\\input\" " ++ texName                  -- tex source
                      ) 
                      Nothing
                      Nothing
                      CreatePipe
                      CreatePipe
                      CreatePipe
                      False
                      False
                      False
