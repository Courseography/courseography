module Main where

import System.IO
import Data.List (isSuffixOf)

import Text.XML.HaXml.ParseLazy      (xmlParse)
import Text.XML.HaXml.Html.ParseLazy (htmlParse)
import Text.XML.HaXml.Wrappers       (fix2Args)
import Text.PrettyPrint.HughesPJ     (render)
import qualified Text.XML.HaXml.Pretty      as XmlPP
import qualified Text.XML.HaXml.Html.Pretty as HtmlPP

-- This is just a trivial application that reads an XML or HTML document
-- from a file (or stdin) and writes it back to another file (or stdout).
-- It demonstrates the behaviour of the parser and pretty-printer,
-- including any shortcomings they may have.

main :: IO ()
main =
  fix2Args >>= \(inf,outf)->
  ( if inf=="-" then getContents
    else readFile inf )            >>= \content->
  ( if outf=="-" then return stdout
    else openFile outf WriteMode ) >>= \o->
  let (parse,format) =
         if ".html" `isSuffixOf` inf || ".htm" `isSuffixOf` inf
           then (htmlParse inf, HtmlPP.document)
           else (xmlParse  inf, XmlPP.document)
  in
  do ( mapM_ (hPutStrLn o) . lines . render . format . parse) content
     hFlush o
