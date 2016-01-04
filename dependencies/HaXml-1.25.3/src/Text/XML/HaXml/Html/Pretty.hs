-- | This is a separate pretty-printer for HTML documents, recognising
--   some of the differences between HTML and true XML.

module Text.XML.HaXml.Html.Pretty
  ( document
  , element
  , attribute
  , content
  ) where

import Prelude hiding (maybe,either)
import Data.Maybe hiding (maybe)
import Data.List (intersperse)
import Data.Char (isSpace)
import Text.PrettyPrint.HughesPJ
import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces

either :: (a->c) -> (b->c) -> Either a b -> c
either  f _g (Left x)  = f x
either _f  g (Right x) = g x

maybe :: (a->Doc) -> Maybe a -> Doc
maybe _f Nothing  = empty
maybe  f (Just x) = f x

--peref p   = text "%" <> text p <> text ";"

----

document :: Document i -> Doc
prolog   :: Prolog -> Doc
xmldecl  :: XMLDecl -> Doc
misc     :: Misc -> Doc
sddecl   :: Bool -> Doc

doctypedecl :: DocTypeDecl -> Doc
markupdecl  :: MarkupDecl -> Doc
--extsubset   :: ExtSubset -> Doc
--extsubsetdecl :: ExtSubsetDecl -> Doc

element   :: Element i -> Doc
attribute :: Attribute -> Doc                     --etc
content   :: Content i -> Doc

----

document (Document p _ e m)= prolog p $$ element e $$ vcat (map misc m)
prolog (Prolog x m1 dtd m2)= maybe xmldecl x $$
                             vcat (map misc m1) $$
                             maybe doctypedecl dtd $$
                             vcat (map misc m2)
xmldecl (XMLDecl v e sd)   = text "<?xml version='" <> text v <> text "'" <+>
                             maybe encodingdecl e <+>
                             maybe sddecl sd <+>
                             text "?>"
misc (Comment s)           = text "<!--" <+> text s <+> text "-->"
misc (PI (n,s))            = text "<?" <> text n <+> text s <+> text "?>"
sddecl sd   | sd           = text "standalone='yes'"
            | otherwise    = text "standalone='no'"
doctypedecl (DTD n eid ds) = if null ds then 
                                  hd <> text ">"
                             else hd <+> text " [" $$
                                  vcat (map markupdecl ds) $$ text "]>"
                           where hd = text "<!DOCTYPE" <+> qname n <+>
                                      maybe externalid eid
markupdecl (Element e)     = elementdecl e
markupdecl (AttList a)     = attlistdecl a
markupdecl (Entity e)      = entitydecl e
markupdecl (Notation n)    = notationdecl n
markupdecl (MarkupMisc m)  = misc m
--markupdecl (MarkupPE p m)  = peref p

--extsubset (ExtSubset t ds) = maybe textdecl t $$
--                             vcat (map extsubsetdecl ds)
--extsubsetdecl (ExtMarkupDecl m)      = markupdecl m
--extsubsetdecl (ExtConditionalSect c) = conditionalsect c
-- --extsubsetdecl (ExtPEReference p e)   = peref p

element (Elem n as []) = text "<" <> qname n <+>
                         fsep (map attribute as) <> text "/>"
element e@(Elem n as cs)
--  | any isText cs    = text "<" <> qname n <+> fsep (map attribute as) <>
--                       text ">" <> hcat (map content cs) <>
--                       text "</" <> qname n <> text ">"
    | isText (head cs) = text "<" <> qname n <+> fsep (map attribute as) <>
                         text ">" <> hcat (map content cs) <>
                         text "</" <> qname n <> text ">"
    | otherwise        = let (d,c) = carryelem e empty
                         in d <> c

isText :: Content i -> Bool
isText (CString _ _ _) = True
isText (CRef _ _)      = True
isText _               = False

carryelem :: Element i -> Doc -> (Doc, Doc)
carryelem (Elem n as []) c
                       = ( c <>
                           text "<" <> qname n <+> fsep (map attribute as)
                         , text "/>")
--carryelem e@(Elem n as cs) c
----  | any isText cs    =  ( c <> element e, empty)
--    | otherwise        =  let (cs',d') = carryscan carrycontent cs (text ">")
--                          in
--                          ( c <>
--                            text "<" <> qname n <+> fsep (map attribute as) $$
--                            nest 2 (vcat cs') <> -- $$
--                            c' <> text "</" <> qname n
--                          , text ">")
--carrycontent (CElem e) c   = carryelem e c
--carrycontent (CString _ s) c = (c <> chardata s, empty)
--carrycontent (CRef r) c    = (c <> reference r, empty)
--carrycontent (CMisc m) c   = (c <> misc m, empty)
--
--carryscan :: (a->c->(b,c)) -> [a] -> c -> ([b],c)
--carryscan f []     c = ([],c)
--carryscan f (a:as) c = let (b, c')   = f a c
--                           (bs,c'') = carryscan f as c'
--                       in (b:bs, c'')

carryelem (Elem n as cs) c
  | isText (head cs) =
        ( start <>
          text ">" <> hcat (map content cs) <> text "</" <> qname n
        , text ">")
  | otherwise =
        let (d,c') = foldl carrycontent (start, text ">") cs in
        ( d <> c' <> text "</" <> qname n
        , text ">")
  where start = c <> text "<" <> qname n <+> fsep (map attribute as)

carrycontent :: (Doc, Doc) -> Content i -> (Doc, Doc)
carrycontent (d,c) (CElem e _)     = let (d',c') = carryelem e c in
                                     (d $$ nest 2 d',       c')
carrycontent (d,c) (CString _ s _) = (d <> c <> chardata s, empty)
carrycontent (d,c) (CRef r _)      = (d <> c <> reference r,empty)
carrycontent (d,c) (CMisc m _)     = (d $$ c <> misc m,     empty)


attribute (n,v)          = qname n <> text "=" <> attvalue v
content (CElem e _)      = element e
content (CString _ s _)  = chardata s
content (CRef r _)       = reference r
content (CMisc m _)      = misc m

elementdecl	:: ElementDecl -> Doc
contentspec	:: ContentSpec -> Doc
cp		:: CP -> Doc
modifier	:: Modifier -> Doc
mixed		:: Mixed -> Doc
attlistdecl	:: AttListDecl -> Doc
attdef		:: AttDef -> Doc
atttype		:: AttType -> Doc
tokenizedtype	:: TokenizedType -> Doc
enumeratedtype	:: EnumeratedType -> Doc
notationtype	:: [String] -> Doc
enumeration	:: [String] -> Doc
defaultdecl	:: DefaultDecl -> Doc
reference	:: Reference -> Doc
entityref	:: String -> Doc
charref		:: (Show a) => a -> Doc
entitydecl	:: EntityDecl -> Doc
gedecl		:: GEDecl -> Doc
pedecl		:: PEDecl -> Doc
entitydef	:: EntityDef -> Doc
pedef		:: PEDef -> Doc
externalid	:: ExternalID -> Doc
ndatadecl	:: NDataDecl -> Doc
notationdecl	:: NotationDecl -> Doc
publicid	:: PublicID -> Doc
encodingdecl	:: EncodingDecl -> Doc
nmtoken		:: String -> Doc
attvalue	:: AttValue -> Doc
entityvalue	:: EntityValue -> Doc
ev		:: EV -> Doc
pubidliteral	:: PubidLiteral -> Doc
systemliteral	:: SystemLiteral -> Doc
chardata	:: [Char] -> Doc


elementdecl (ElementDecl n cs) = text "<!ELEMENT" <+> qname n <+>
                                 contentspec cs <> text ">"
contentspec EMPTY              = text "EMPTY"
contentspec ANY                = text "ANY"
contentspec (Mixed m)          = mixed m
contentspec (ContentSpec c)    = cp c
--contentspec (ContentPE p cs)   = peref p
cp (TagName n m)       = qname n <> modifier m
cp (Choice cs m)       = parens (hcat (intersperse (text "|") (map cp cs))) <>
                           modifier m
cp (Seq cs m)          = parens (hcat (intersperse (text ",") (map cp cs))) <>
                           modifier m
--cp (CPPE p c)          = peref p
modifier None          = empty
modifier Query         = text "?"
modifier Star          = text "*"
modifier Plus          = text "+"
mixed  PCDATA          = text "(#PCDATA)"
mixed (PCDATAplus ns)  = text "(#PCDATA |" <+>
                         hcat (intersperse (text "|") (map qname ns)) <>
                         text ")*"

attlistdecl (AttListDecl n ds) = text "<!ATTLIST" <+> qname n <+>
                                 fsep (map attdef ds) <> text ">"
attdef (AttDef n t d)          = qname n <+> atttype t <+> defaultdecl d
atttype  StringType            = text "CDATA"
atttype (TokenizedType t)      = tokenizedtype t
atttype (EnumeratedType t)     = enumeratedtype t
tokenizedtype ID               = text "ID"
tokenizedtype IDREF            = text "IDREF"
tokenizedtype IDREFS           = text "IDREFS"
tokenizedtype ENTITY           = text "ENTITY"
tokenizedtype ENTITIES         = text "ENTITIES"
tokenizedtype NMTOKEN          = text "NMTOKEN"
tokenizedtype NMTOKENS         = text "NMTOKENS"
enumeratedtype (NotationType n)= notationtype n
enumeratedtype (Enumeration e) = enumeration e
notationtype ns                = text "NOTATION" <+>
                                 parens (hcat (intersperse (text "|") (map text ns)))
enumeration ns                 = parens (hcat (intersperse (text "|") (map nmtoken ns)))
defaultdecl  REQUIRED          = text "#REQUIRED"
defaultdecl  IMPLIED           = text "#IMPLIED"
defaultdecl (DefaultTo a f)    = maybe (const (text "#FIXED")) f <+> attvalue a
--conditionalsect (IncludeSect i)= text "<![INCLUDE [" <+>
--                                 vcat (map extsubsetdecl i) <+> text "]]>"
--conditionalsect (IgnoreSect i) = text "<![IGNORE [" <+>
--                                 fsep (map ignoresectcontents i) <+> text "]]>"
--ignore (Ignore)                = empty
--ignoresectcontents (IgnoreSectContents i is)
--                               = ignore i <+> vcat (map internal is)
--                          where internal (ics,i) = text "<![[" <+>
--                                                   ignoresectcontents ics <+>
--                                                   text "]]>" <+> ignore i
reference (RefEntity er)       = entityref er
reference (RefChar cr)         = charref cr
entityref n                    = text "&" <> text n <> text ";"
charref c                      = text "&#" <> text (show c) <> text ";"
entitydecl (EntityGEDecl d)    = gedecl d
entitydecl (EntityPEDecl d)    = pedecl d
gedecl (GEDecl n ed)           = text "<!ENTITY" <+> text n <+> entitydef ed <>
                                 text ">"
pedecl (PEDecl n pd)           = text "<!ENTITY %" <> text n <+> pedef pd <>
                                 text ">"
entitydef (DefEntityValue ev)  = entityvalue ev
entitydef (DefExternalID i nd) = externalid i <+> maybe ndatadecl nd
pedef (PEDefEntityValue ev)    = entityvalue ev
pedef (PEDefExternalID eid)    = externalid eid
externalid (SYSTEM sl)         = text "SYSTEM" <+> systemliteral sl
externalid (PUBLIC i sl)       = text "PUBLIC" <+> pubidliteral i <+>
                                 systemliteral sl
ndatadecl (NDATA n)            = text "NDATA" <+> text n
--textdecl (TextDecl vi ed)      = text "<?xml" <+> maybe text vi <+>
--                                 encodingdecl ed <> text "?>"
--extparsedent (ExtParsedEnt t c)= maybe textdecl t <+> content c
--extpe (ExtPE t esd)            = maybe textdecl t <+>
--                                 vcat (map extsubsetdecl esd)
notationdecl (NOTATION n e)    = text "<!NOTATION" <+> text n <+>
                                 either externalid publicid e <>
                                 text ">"
publicid (PUBLICID p)          = text "PUBLICID" <+> pubidliteral p
encodingdecl (EncodingDecl s)  = text "encoding='" <> text s <> text "'"
nmtoken s                      = text s
attvalue (AttValue esr)        = text "\"" <>
                                 hcat (map (either text reference) esr) <>
                                 text "\""
entityvalue (EntityValue evs)  = text "'" <> hcat (map ev evs) <> text "'"
ev (EVString s)                = text s
--ev (EVPERef p e)               = peref p
ev (EVRef r)                   = reference r
pubidliteral (PubidLiteral s)  = text "'" <> text s <> text "'"
systemliteral (SystemLiteral s)= text "'" <> text s <> text "'"
chardata s                     = if all isSpace s then empty else text s
--cdsect c                       = text "<![CDATA[" <> chardata c <> text "]]>"

qname n                        = text (printableName n)

----
