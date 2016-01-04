{-# LANGUAGE CPP #-}
#define dummy		-- just to ensure that cpp gets called on this file
module Text.XML.HaXml.Wrappers
  ( fix2Args
  , processXmlWith
  , onContent
  ) where

-- imports required for processXmlWith and fix2Args
import Prelude hiding (filter)
import System.Exit
import System.Environment
import System.IO
import Data.List  (isSuffixOf)
import Control.Monad (when)

import Text.XML.HaXml.Types       (Document(..),Content(..))
import Text.XML.HaXml.Combinators (CFilter)
import Text.XML.HaXml.Posn        (Posn,posInNewCxt)
import Text.XML.HaXml.Parse       (xmlParse)
import Text.XML.HaXml.Html.Parse  (htmlParse)
import Text.XML.HaXml.Pretty as PP(document)
import Text.PrettyPrint.HughesPJ  (render)


-- | This useful auxiliary checks the commandline arguments for two
--   filenames, the input and output file respectively.  If either
--   is missing, it is replaced by '-', which can be interpreted by the
--   caller as stdin and\/or stdout.
fix2Args :: IO (String,String)
fix2Args = do
  args <- getArgs
  when ("--version" `elem` args) $ do
      putStrLn $ "part of HaXml-"++ VERSION
      exitWith ExitSuccess
  when ("--help" `elem` args) $ do
      putStrLn $ "See http://projects.haskell.org/HaXml"
      exitWith ExitSuccess
  case length args of
    0 -> return ("-",     "-")
    1 -> return (args!!0, "-")
    2 -> return (args!!0, args!!1)
    _ -> do prog <- getProgName
            putStrLn ("Usage: "++prog++" [infile] [outfile]")
            exitFailure


-- | The wrapper @processXmlWith@ returns an IO () computation
--   that collects the filenames (or stdin\/stdout) to use when
--   reading\/writing XML documents.  Its CFilter argument
--   is applied to transform the XML document from the input and
--   write it to the output.  No DTD is attached to the output.
--
--   If the input filename ends with .html or .htm, it is parsed using
--   the error-correcting HTML parser rather than the strict XML parser.
processXmlWith :: CFilter Posn -> IO ()
processXmlWith f = do
  (inf,outf) <- fix2Args
  input      <- if inf=="-" then getContents else readFile inf
  o          <- if outf=="-" then return stdout else openFile outf WriteMode
  parse      <- if ".html" `isSuffixOf` inf || ".htm" `isSuffixOf` inf
                then return (htmlParse inf)
                else return (xmlParse inf)
  ( hPutStrLn o . render . PP.document . onContent inf f . parse ) input
  hFlush o

  where
    onContent :: FilePath -> (CFilter Posn) -> Document Posn -> Document Posn
    onContent file filter (Document p s e m) =
        case filter (CElem e (posInNewCxt file Nothing)) of
            [CElem e' _] -> Document p s e' m
            []           -> error $ "filtering"++file++"produced no output"
            _            -> error $ "filtering"++file++
                                    "produced more than one output document"

-- | The wrapper @onContent@ simply applies a given content filter to a
--   document.  Ambiguous or empty results raise an error exception.
onContent :: CFilter i -> Document i -> Document i
onContent filter (Document p s e m) =
    case filter (CElem e undefined) of
      [CElem e' _] -> Document p s e' m
      []           -> error "onContent: produced no output"
      _            -> error "onContent: produced more than one output"
