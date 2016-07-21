{-# LANGUAGE OverloadedStrings #-}

module Response.Export
    (exportResponse) where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Monad.IO.Class  (liftIO)
import Happstack.Server
import System.Process
import GHC.IO.Handle.Types
import ImageConversion

-- | Serves a pdf file requested by the user.
exportResponse :: ServerPart Response
exportResponse = do
	-- TODO: replace all instances of "test" with image file
	let pdfName = "test.pdf"
	    texName = "test.tex"
	liftIO $ createPDF texName pdfName "test"
	serveFile (asContentType "application/pdf") pdfName

-- | Create a process to use the pdflatex program to create a .pdf file from a .tex file
convertTexToPDF :: String -> String -> String -> IO
							                     (Maybe Handle,
							                      Maybe Handle,
							                      Maybe Handle,
							                      ProcessHandle)

convertTexToPDF texName pdfName imgName = createProcess $ CreateProcess
											(ShellCommand $ "pdflatex " ++
															"\'\\def\\img{" ++
															imgName ++
															"}\\input " ++
															texName ++
															" " ++
															pdfName ++ "\'"
											) -- runs pdflatex from shell command
											Nothing
											Nothing
											CreatePipe
											CreatePipe
											CreatePipe
											False
											False
											False

-- | Opens a new process to create a .pdf file from a .tex file and waits for that process to terminate.
createPDF :: String -> String -> String -> IO ()
createPDF texName pdfName imgName =	do
		-- Get the pid of process that is opened for converting .tex to .pdf
		(_, _, _, pid) <- convertTexToPDF texName pdfName imgName
		print "Waiting for a process..."
		-- Force current process to wait for conversion to finish
		waitForProcess pid
		print "Process Complete"


