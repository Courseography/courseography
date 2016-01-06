module Main where

-- This program is provided to convert an XML file containing a DTD
-- into a Haskell module containing data/newtype definitions which
-- mirror the DTD.  Once you have used this program to generate your type
-- definitions, you should import Xml2Haskell wherever you intend
-- to read and write XML files with your Haskell programs.

import System.Environment
import System.Exit
import System.IO
import Data.List (nub,takeWhile,dropWhile)
import Control.Monad

--import Text.XML.HaXml.Wrappers   (fix2Args)
import Text.XML.HaXml            (version)
import Text.XML.HaXml.Types      (DocTypeDecl(..))
import Text.XML.HaXml.Namespaces (localName)
import Text.XML.HaXml.Parse      (dtdParse)
import Text.XML.HaXml.DtdToHaskell.TypeDef  (TypeDef,ppTypeDef,mangle)
import Text.XML.HaXml.DtdToHaskell.Convert  (dtd2TypeDef)
import Text.XML.HaXml.DtdToHaskell.Instance (mkInstance)
import Text.PrettyPrint.HughesPJ (render,vcat)

-- sucked in from Text.XML.HaXml.Wrappers to avod dependency on T.X.H.Html
fix2Args :: IO (String,String)
fix2Args = do
  args <- getArgs
  when ("--version" `elem` args) $ do
      putStrLn $ "part of HaXml-"++version
      exitWith ExitSuccess
  when ("--help" `elem` args) $ do
      putStrLn $ "See http://haskell.org/HaXml"
      exitWith ExitSuccess
  case length args of
    0 -> return ("-",     "-")
    1 -> return (args!!0, "-")
    2 -> return (args!!0, args!!1)
    _ -> do prog <- getProgName
            putStrLn ("Usage: "++prog++" [xmlfile] [outfile]")
            exitFailure


main ::IO ()
main =
  fix2Args >>= \(inf,outf)->
  ( if inf=="-" then getContents
    else readFile inf )           >>= \content->
  ( if outf=="-" then return stdout
    else openFile outf WriteMode ) >>= \o->
  let (DTD name _ markup) = (getDtd . dtdParse inf) content
      decls = (nub . dtd2TypeDef) markup
      realname = if outf/="-" then mangle (trim outf)
                 else if null (localName name) then mangle (trim inf)
                 else mangle (localName name)
  in
  do hPutStrLn o ("module "++realname
                  ++" where\n\nimport Text.XML.HaXml.XmlContent"
                  ++"\nimport Text.XML.HaXml.Types"
                  ++"\nimport Text.XML.HaXml.OneOfN")
    --            ++"\nimport Char (isSpace)"
    --            ++"\nimport List (isPrefixOf)"
     hPutStrLn o "\n\n{-Type decls-}\n"
     (hPutStrLn o . render . vcat . map ppTypeDef) decls
     hPutStrLn o "\n\n{-Instance decls-}\n"
     mapM_ (hPutStrLn o . (++"\n") . render . mkInstance) decls
     hPutStrLn o "\n\n{-Done-}"
     hFlush o

getDtd :: Maybe t -> t
getDtd (Just dtd) = dtd
getDtd (Nothing)  = error "No DTD in this document"

trim :: [Char] -> [Char]
trim name | '/' `elem` name  = (trim . tail . dropWhile (/='/')) name
          | '.' `elem` name  = takeWhile (/='.') name
          | otherwise        = name

