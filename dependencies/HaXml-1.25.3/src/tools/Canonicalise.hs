module Main where

import System.IO
import Data.List   (isSuffixOf)

import Text.XML.HaXml.Parse      (xmlParse)
import Text.XML.HaXml.Html.Parse (htmlParse)
import Text.XML.HaXml.Pretty     (document)
import Text.XML.HaXml.Wrappers   (fix2Args)
import Text.PrettyPrint.HughesPJ (render)

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
  let parse = if ".html" `isSuffixOf` inf || ".htm" `isSuffixOf` inf
              then htmlParse inf else xmlParse inf
  in
  do ( hPutStrLn o . render . document . parse) content
     hFlush o

