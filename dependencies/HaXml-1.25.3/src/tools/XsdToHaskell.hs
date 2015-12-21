-- XsdToHaskell
module Main where

-- This program is designed to convert an XML file containing an XSD
-- decl into a Haskell module containing data/newtype definitions which
-- mirror the XSD.  Once you have used this program to generate your type
-- definitions, you should import Xsd2Haskell wherever you intend
-- to read and write XML files with your Haskell programs.

import System.Environment
import System.Exit
import System.IO
import Control.Monad
--import Data.Either

--import Text.XML.HaXml.Wrappers   (fix2Args)
import Text.XML.HaXml            (version)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces (resolveAllNames,qualify
                                 ,nullNamespace)
import Text.XML.HaXml.Parse      (xmlParse')
import Text.XML.HaXml.Util       (docContent)
import Text.XML.HaXml.Posn       (posInNewCxt)

import Text.XML.HaXml.Schema.Parse
import Text.XML.HaXml.Schema.Environment
import Text.XML.HaXml.Schema.NameConversion
import Text.XML.HaXml.Schema.TypeConversion
import Text.XML.HaXml.Schema.PrettyHaskell
import qualified Text.XML.HaXml.Schema.HaskellTypeModel as Haskell
import Text.ParserCombinators.Poly
import Text.PrettyPrint.HughesPJ (render,vcat)

-- sucked in from Text.XML.HaXml.Wrappers to avoid dependency on T.X.H.Html
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
    else readFile inf )           >>= \thiscontent->
  ( if outf=="-" then return stdout
    else openFile outf WriteMode ) >>= \o->
  let d@Document{} = resolveAllNames qualify
                     . either (error . ("not XML:\n"++)) id
                     . xmlParse' inf
                     $ thiscontent
  in do
    case runParser schema [docContent (posInNewCxt inf Nothing) d] of
        (Left msg,_) ->    hPutStrLn stderr msg
        (Right v,[]) -> do hPutStrLn stdout $ "Parse Success!"
                           hPutStrLn stdout $ "\n-----------------\n"
                           hPutStrLn stdout $ show v
                           hPutStrLn stdout $ "\n-----------------\n"
                           let decls = convert (mkEnvironment inf v emptyEnv) v
                               haskl = Haskell.mkModule inf v decls
                               doc   = ppModule simpleNameConverter haskl
                           hPutStrLn stdout $ render doc
        (Right v,_)  -> do hPutStrLn stdout $ "Parse incomplete!"
                           hPutStrLn stdout $ "\n-----------------\n"
                           hPutStrLn stdout $ show v
                           hPutStrLn stdout $ "\n-----------------\n"
    hFlush o

  
--do hPutStrLn o $ "Document contains XSD for target namespace "++
--                 targetNamespace e
  {-
  let (DTD name _ markup) = (getDtd . dtdParse inf) content
      decls = (nub . dtd2TypeDef) markup
      realname = if outf/="-" then mangle (trim outf)
                 else if null (localName name) then mangle (trim inf)
                 else mangle (localName name)
  in
  do hPutStrLn o ("module "++realname
                  ++" where\n\nimport Text.XML.HaXml.XmlContent"
                  ++"\nimport Text.XML.HaXml.OneOfN")
    --            ++"\nimport Char (isSpace)"
    --            ++"\nimport List (isPrefixOf)"
     hPutStrLn o "\n\n{-Type decls-}\n"
     (hPutStrLn o . render . vcat . map ppTypeDef) decls
     hPutStrLn o "\n\n{-Instance decls-}\n"
     mapM_ (hPutStrLn o . (++"\n") . render . mkInstance) decls
     hPutStrLn o "\n\n{-Done-}"
     hFlush o
  -}

{-
getDtd :: Maybe t -> t
getDtd (Just dtd) = dtd
getDtd (Nothing)  = error "No DTD in this document"

trim :: [Char] -> [Char]
trim name | '/' `elem` name  = (trim . tail . dropWhile (/='/')) name
          | '.' `elem` name  = takeWhile (/='.') name
          | otherwise        = name
-}

targetNamespace :: Element i -> String
targetNamespace (Elem qn attrs _) =
    if qn /= xsdSchema then "ERROR! top element not an xsd:schema tag"
    else case lookup (N "targetNamespace") attrs of
           Nothing -> "ERROR! no targetNamespace specified"
           Just atv -> show atv

xsdSchema :: QName
xsdSchema = QN (nullNamespace{nsURI="http://www.w3.org/2001/XMLSchema"})
               "schema"

--  <xsd:schema xmlns:xsd="" xmlns:fpml="" targetNamespace="" version=""
--              attributeFormDefault="unqualified"
--              elementFormDefault="qualified">
