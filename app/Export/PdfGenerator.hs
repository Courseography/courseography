{-|
    Module      : Export.PdfGenerator
    Description : Contains functions for generating a PDF from LaTeX text.
-}
module Export.PdfGenerator
    (createPDF) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import GHC.IO.Handle.Types
import System.IO (hClose)
import System.Process (ProcessHandle, StdStream (CreatePipe), createProcess, proc, std_in,
                       waitForProcess)

-- | Opens a new process to create a PDF from a TEX
createPDF :: Text -> FilePath -> String -> IO ()
createPDF texText outDir jobName = do
    (Just hin, _, _, pid) <- convertTexToPDF outDir jobName
    TIO.hPutStr hin texText
    hClose hin
    _ <- waitForProcess pid
    return ()

-- | Create a process to use the pdflatex program to create a PDF from a TEX
-- read from stdin. The process is run in nonstop mode and so it will not block
-- if an error occurs. The resulting PDF will have the same filename as jobName.
convertTexToPDF :: FilePath -> String -> IO
                                        (Maybe Handle,
                                         Maybe Handle,
                                         Maybe Handle,
                                         ProcessHandle)
convertTexToPDF outDir jobName =
    createProcess (proc "pdflatex"
      [ "-interaction=nonstopmode"
      , "-output-directory=" ++ outDir
      , "-jobname=" ++ jobName
      ]) { std_in = CreatePipe }
