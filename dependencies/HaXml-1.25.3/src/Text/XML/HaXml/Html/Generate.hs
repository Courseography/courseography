-- | These are just some common abbreviations for generating HTML
--   content within the XML transformation framework defined
--   by "Text.Xml.HaXml.Combinators".
module Text.XML.HaXml.Html.Generate
  ( -- * HTML construction filters
  -- ** Containers
    html
  , hhead
  , htitle
  , hbody
  , h1, h2, h3, h4
  , hpara
  , hdiv, hspan, margin
  -- ** Anchors
  , anchor, makehref, anchorname
  -- ** Text style
  , hpre
  , hcentre
  , hem, htt, hbold
  , parens, bullet
  -- ** Tables
  , htable, hrow, hcol
  -- ** Breaks, lines
  , hbr, hhr
  -- ** Attributes
  , showattr, (!), (?)
  -- * A simple HTML pretty-printer
  , htmlprint
  ) where

import Data.Char (isSpace)

import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces
import Text.XML.HaXml.Combinators
import qualified Text.PrettyPrint.HughesPJ as Pretty

---- Constructor functions

html, hhead, htitle, hbody, h1, h2, h3, h4, hpara, hpre, hcentre,
    hem, htt, hbold, htable, hrow, hcol, hdiv, hspan, margin
       :: [CFilter i] -> CFilter i
html    = mkElem "html"
hhead   = mkElem "head"
htitle  = mkElem "title"
hbody   = mkElem "body"
h1      = mkElem "h1"
h2      = mkElem "h2"
h3      = mkElem "h3"
h4      = mkElem "h4"
hpara   = mkElem "p"
hpre    = mkElem "pre"
hcentre = mkElem "center"
hem     = mkElem "em"
htt     = mkElem "tt"
hbold   = mkElem "b"

htable = mkElem "table"
hrow   = mkElem "tr"
hcol   = mkElem "td"

hdiv   = mkElem "div"
hspan  = mkElem "span"
margin = mkElemAttr "div" [("margin-left",("2em"!)),
                           ("margin-top", ("1em"!))]

anchor      :: [(String, CFilter i)] -> [CFilter i] -> CFilter  i
anchor       = mkElemAttr "a"

makehref, anchorname :: CFilter i -> [CFilter i] -> CFilter i
makehref r   = anchor [ ("href",r) ]
anchorname n = anchor [ ("name",n) ]


hbr, hhr :: CFilter i
hbr       = mkElem "br" []
hhr       = mkElem "hr" []


showattr, (!), (?) :: String -> CFilter i
showattr n = find n literal
(!) = literal
(?) = showattr

parens :: CFilter i -> CFilter i
parens f = cat [ literal "(", f, literal ")" ]

bullet :: [CFilter i] -> CFilter i
bullet = cat . (literal "M-^U":)


---- Printing function

-- htmlprint :: [Content] -> String
-- htmlprint = concatMap cprint
--   where
--   cprint (CElem e _) = elem e
--   cprint (CString _ s) = s
--   cprint (CMisc m) = ""
--
--   elem (Elem n as []) = "\n<"++n++attrs as++" />"
--   elem (Elem n as cs) = "\n<"++n++attrs as++">"++htmlprint cs++"\n</"++n++">"
--
--   attrs = concatMap attr
--   attr (n,v) = " "++n++"='"++v++"'"


htmlprint :: [Content i] -> Pretty.Doc
htmlprint = Pretty.cat . map cprint . foldrefs
  where
  foldrefs [] = []
  foldrefs (CString ws s1 i:CRef r _:CString _ s2 _:cs) =
              CString ws (s1++"&"++ref r++";"++s2) i: foldrefs cs
  foldrefs (c:cs) = c : foldrefs cs

--ref (RefEntity (EntityRef n)) = n     -- Actually, should look-up symtable.
--ref (RefChar (CharRef s)) = s
  ref (RefEntity n) = n -- Actually, should look-up symtable.
  ref (RefChar s) = show s

  cprint (CElem e _)      = element e
  cprint (CString ws s _) = Pretty.cat (map Pretty.text (fmt 60
                                             ((if ws then id else deSpace) s)))
  cprint (CRef r _)       = Pretty.text ("&"++ref r++";")
  cprint (CMisc _ _)      = Pretty.empty

  element (Elem n as []) = Pretty.text "<"               Pretty.<>
                           Pretty.text (printableName n) Pretty.<>
                           attrs as                      Pretty.<>
                           Pretty.text " />"
  element (Elem n as cs) =
                    --  ( Pretty.text "<"   Pretty.<>
                    --    Pretty.text n     Pretty.<>
                    --    attrs as          Pretty.<>
                    --    Pretty.text ">")  Pretty.$$
                    --  Pretty.nest 6 (htmlprint cs)  Pretty.$$
                    --  ( Pretty.text "</"  Pretty.<>
                    --    Pretty.text n     Pretty.<>
                    --    Pretty.text ">" )
                        Pretty.fcat [ ( Pretty.text "<"               Pretty.<>
                                        Pretty.text (printableName n) Pretty.<>
                                        attrs as                      Pretty.<>
                                        Pretty.text ">")
                                    , Pretty.nest 4 (htmlprint cs)
                                    , ( Pretty.text "</"              Pretty.<>
                                        Pretty.text (printableName n) Pretty.<>
                                        Pretty.text ">" )
                                    ]

  attrs = Pretty.cat . map attribute
  attribute (n,v@(AttValue _)) =
               Pretty.text " "               Pretty.<>
               Pretty.text (printableName n) Pretty.<>
               Pretty.text "='"              Pretty.<>
               Pretty.text (show v)          Pretty.<>
               Pretty.text "'"

  fmt _ [] = []
  fmt n s  = let (top,bot) = splitAt n s
                 (word,left) = keepUntil isSpace (reverse top)
             in if length top < n then [s]
                else if not (null left) then
                     reverse left: fmt n (word++bot)
                else let (big,rest) = keepUntil isSpace s
                     in reverse big: fmt n rest

  deSpace []     = []
  deSpace (c:cs) | c=='\n'   = deSpace (' ':cs)
                 | isSpace c = c : deSpace (dropWhile isSpace cs)
                 | otherwise = c : deSpace cs

  keepUntil p xs = select p ([],xs)
      where select _ (ls,[])     = (ls,[])
            select q (ls,(y:ys)) | q y       = (ls,y:ys)
                                 | otherwise = select q (y:ls,ys)

