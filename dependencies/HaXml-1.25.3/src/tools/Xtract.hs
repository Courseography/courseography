------------------------------------------------------------
-- The Xtract tool - an XML-grep.
------------------------------------------------------------ 
module Main where
import System.Environment (getArgs)
import System.Exit        (exitWith, ExitCode(..))
import System.IO
import Data.Char          (toLower)
import Data.List          (isSuffixOf)
import Control.Monad      (when)
import System.Console.GetOpt

import Text.XML.HaXml                          (version)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn                     (posInNewCxt)
import qualified Text.XML.HaXml.Parse          (xmlParse)
import qualified Text.XML.HaXml.Html.Parse     (htmlParse)
import Text.XML.HaXml.Xtract.Parse             (xtract)
import qualified Text.XML.HaXml.ParseLazy      (xmlParse)
import qualified Text.XML.HaXml.Html.ParseLazy (htmlParse)
import Text.PrettyPrint.HughesPJ               (Doc,render, vcat, hcat, empty)
import Text.XML.HaXml.Pretty                   (content)
import Text.XML.HaXml.Html.Generate            (htmlprint)
import Text.XML.HaXml.Escape                   (xmlEscapeContent,stdXmlEscaper)
import Text.XML.HaXml.Util                     (docContent)

escape :: [Content i] -> [Content i]
escape = xmlEscapeContent stdXmlEscaper

data Opts = Opts {doEscaping :: Bool, forceHtml :: Bool, printHelp :: Bool, printVersion :: Bool, beLazy :: Bool}

defaultOptions = Opts {doEscaping = True, forceHtml = False, printHelp = False, printVersion = False, beLazy = False}

options :: [OptDescr (Opts -> Opts)]
options = [
    Option ['n'] []
        (NoArg (\o -> o {doEscaping = False})) "Do not escape output",
    Option [] ["html"]
        (NoArg (\o -> o {forceHtml = True})) "Force HTML mode",
    Option [] ["help"]
        (NoArg (\o -> o {printHelp = True})) "Displays this help",
    Option [] ["version"]
        (NoArg (\o -> o {printVersion = True})) "Prints version",
    Option ['l'] ["lazy"]
        (NoArg (\o -> o {beLazy = True})) "Parse lazily"
    ]

main :: IO ()
main = do
  preArgs <- getArgs
  let (preOpts, args, errs) = getOpt Permute options preArgs
  let opts = foldl (flip ($)) defaultOptions preOpts
  when (printVersion opts) $ do
      putStrLn $ "part of HaXml-"++version
      exitWith ExitSuccess
  when (printHelp opts) $ do
      putStrLn $ "See http://haskell.org/HaXml"
      exitWith ExitSuccess
  when (length args < 1) $ do
      putStrLn $ usageInfo "Usage: Xtract [options] <pattern> [xmlfile ...]" options
      exitWith (ExitFailure 1)
  let (xmlParse, htmlParse) = if beLazy opts then 
        (Text.XML.HaXml.ParseLazy.xmlParse, Text.XML.HaXml.Html.ParseLazy.htmlParse) 
        else 
        (Text.XML.HaXml.Parse.xmlParse, Text.XML.HaXml.Html.Parse.htmlParse)
  let (pattern,files,esc) =
          (head args,tail args,if doEscaping opts then escape .(:[]) else (:[]))
--      findcontents =
--        if null files then (getContents >>= \x-> return [xmlParse "<stdin>"x])
--        else mapM (\x-> do c <- (if x=="-" then getContents else readFile x)
--                           return ((if isHTML x
--                                    then htmlParse x else xmlParse x) c))
--                  files
--  findcontents >>= \cs->
--  ( hPutStrLn stdout . render . vcat
--  . map (vcat . map content . selection . docContent)) cs
  mapM_ (\x->   do c <- (if x=="-" then getContents else readFile x)
                   ( if isHTML x || forceHtml opts then
                          hPutStrLn stdout . render . htmlprint
                          . xtract (map toLower) pattern
                          . docContent (posInNewCxt x Nothing) . htmlParse x
                     else hPutStrLn stdout . render . vcat . map (format . esc)
                          . xtract id pattern
                          . docContent (posInNewCxt x Nothing) . xmlParse x) c
                   hFlush stdout)
          files

isHTML :: [Char] -> Bool
isHTML x = ".html" `isSuffixOf` x  ||  ".htm"  `isSuffixOf` x

format :: [Content i] -> Doc
format [] = empty
format cs@(CString _ _ _:_) = hcat . map content $ cs
format cs@(CRef _ _:_)      = hcat . map content $ cs
format cs                   = vcat . map content $ cs
