module Text.XML.HaXml.ShowXmlLazy (showXmlLazy) where

import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces
import Text.XML.HaXml.TypeMapping -- (toHType, toDTD, Tuple(), Defined, showHType)

import qualified Text.XML.HaXml.XmlContent as X
import Prelude hiding (maybe,either)
import Data.Maybe hiding (maybe)
import Data.List

-- | Convert a fully-typed XML document to a string (without DTD).
showXmlLazy :: X.XmlContent a => Bool -> a -> String
showXmlLazy dtd x =
    case X.toContents x of
      [CElem _ _] -> document $ toXmlLazy dtd x
      _ -> ""


-- | Convert a fully-typed XML document to a string (with or without DTD).
toXmlLazy :: X.XmlContent a => Bool -> a -> Document ()
toXmlLazy dtd value =
    let ht = toHType value in
    Document (Prolog (Just (XMLDecl "1.0" Nothing Nothing))
                     [] (if dtd then Just (toDTD ht) else Nothing) [])
             emptyST
             ( case (ht, X.toContents value) of
                 (Tuple _, cs)       -> Elem (N $ showHType ht "") [] cs
                 (Defined _ _ _, cs) -> Elem (N $ showHType ht "-XML") [] cs
                 (_, [CElem e ()])   -> e )
             []




type Doc = String
empty :: [Char]
empty = ""
($$) :: [Char] -> [Char] -> [Char]
($$) a b = a ++ "\n" ++ b
vcat :: [[Char]] -> [Char]
vcat a = foldr ($$) [] a
hcat :: [[a]] -> [a]
hcat = concat
(<>) :: [a] -> [a] -> [a]
(<>) = (++)
(<+>) :: [Char] -> [Char] -> [Char]
(<+>) a b = a ++ " " ++ b
fsep :: [[Char]] -> [Char]
fsep = vcat
text :: t -> t
text string = string
parens :: [Char] -> [Char]
parens string = "("++string++")"
nest :: t -> t1 -> t1
nest _ string = string



either :: (t -> t1) -> (t2 -> t1) -> Either t t2 -> t1
either f _ (Left x)  = f x
either _ g (Right x) = g x

maybe :: (t -> [Char]) -> Maybe t -> [Char]
maybe _ Nothing  = empty
maybe f (Just x) = f x

--peref p   = text "%" <> text p <> text ";"

----

document :: Document i -> Doc
prolog   :: Prolog -> Doc
xmldecl  :: XMLDecl -> Doc
misc     :: Misc -> Doc
sddecl   :: Bool -> Doc

doctypedecl :: DocTypeDecl -> Doc
markupdecl  :: MarkupDecl -> Doc
-- extsubset   :: ExtSubset -> Doc
-- extsubsetdecl :: ExtSubsetDecl -> Doc
cp          :: CP -> Doc

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
-- _ (ExtSubset t ds) = maybe textdecl t $$
--                              vcat (map extsubsetdecl ds)
-- _ (ExtMarkupDecl m)      = markupdecl m
-- extsubsetdecl (ExtConditionalSect c) = conditionalsect c
--extsubsetdecl (ExtPEReference p e)   = peref p

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

isText :: Content t -> Bool
isText (CString _ _ _) = True
isText (CRef _ _)      = True
isText _               = False

carryelem (Elem n as []) c
                       = ( c <>
                           text "<" <> qname n <+> fsep (map attribute as)
                         , text "/>")
carryelem (Elem n as cs) c
--  | any isText cs    =  ( c <> element e, empty)
    | otherwise        =  let (cs0,d0) = carryscan carrycontent cs (text ">")
                          in
                          ( c <>
                            text "<" <> qname n <+> fsep (map attribute as) $$
                            nest (2 :: Int) (vcat cs0) <> --- $$
                            d0 <> text "</" <> qname n
                          , text ">")
carrycontent :: Content t -> [Char] -> ([Char], [Char])
carryelem :: Element t -> [Char] -> ([Char], [Char])
carrycontent (CElem e _) c   = carryelem e c
carrycontent (CString False s _) c = (c <> chardata s, empty)
carrycontent (CString True  s _) c = (c <> cdsect s, empty)
carrycontent (CRef r _) c    = (c <> reference r, empty)
carrycontent (CMisc m _) c   = (c <> misc m, empty)

carryscan :: (a->c->(b,c)) -> [a] -> c -> ([b],c)
carryscan _ []     c = ([],c)
carryscan f (a:as) c = let (b, c0) = f a c
                           (bs,c1) = carryscan f as c0
                       in (b:bs, c1)

--carryelem e@(Elem n as cs) c
--  | isText (head cs) =
--        ( start <>
--          text ">" <> hcat (map content cs) <> text "</" <> text n
--        , text ">")
--  | otherwise =
--        let (d,c0) = foldl carrycontent (start, text ">") cs in
--        ( d <> c0 <> text "</" <> text n
--        , text ">")
--  where start = c <> text "<" <> text n <+> fsep (map attribute as)
--
--carrycontent (d,c) (CElem e)   = let (d',c') = carryelem e c in
--                                 (d $$ nest 2 d',       c')
--carrycontent (d,c) (CString _ s) = (d <> c <> chardata s, empty)
--carrycontent (d,c) (CRef r)    = (d <> c <> reference r,empty)
--carrycontent (d,c) (CMisc m)   = (d $$ c <> misc m,     empty)


attribute (n,v)             = qname n <> text "=" <> attvalue v
content (CElem e _)         = element e
content (CString False s _) = chardata s
content (CString True s _)  = cdsect s
content (CRef r _)          = reference r
content (CMisc m _)         = misc m

elementdecl :: ElementDecl -> [Char]
elementdecl (ElementDecl n cs) = text "<!ELEMENT" <+> qname n <+>
                                 contentspec cs <> text ">"
contentspec :: ContentSpec -> [Char]
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
modifier :: Modifier -> [Char]
modifier None          = empty
modifier Query         = text "?"
modifier Star          = text "*"
modifier Plus          = text "+"
mixed :: Mixed -> [Char]
mixed  PCDATA          = text "(#PCDATA)"
mixed (PCDATAplus ns)  = text "(#PCDATA |" <+>
                         hcat (intersperse (text "|") (map qname ns)) <>
                         text ")*"

attlistdecl :: AttListDecl -> [Char]
attlistdecl (AttListDecl n ds) = text "<!ATTLIST" <+> qname n <+>
                                 fsep (map attdef ds) <> text ">"
attdef :: AttDef -> [Char]
attdef (AttDef n t d)          = qname n <+> atttype t <+> defaultdecl d
atttype :: AttType -> [Char]
atttype  StringType            = text "CDATA"
atttype (TokenizedType t)      = tokenizedtype t
atttype (EnumeratedType t)     = enumeratedtype t
tokenizedtype :: TokenizedType -> [Char]
tokenizedtype ID               = text "ID"
tokenizedtype IDREF            = text "IDREF"
tokenizedtype IDREFS           = text "IDREFS"
tokenizedtype ENTITY           = text "ENTITY"
tokenizedtype ENTITIES         = text "ENTITIES"
tokenizedtype NMTOKEN          = text "NMTOKEN"
tokenizedtype NMTOKENS         = text "NMTOKENS"
enumeratedtype :: EnumeratedType -> [Char]
enumeratedtype (NotationType n)= notationtype n
enumeratedtype (Enumeration e) = enumeration e
notationtype :: [[Char]] -> [Char]
notationtype ns                = text "NOTATION" <+>
                                 parens (hcat (intersperse (text "|") (map text ns)))
enumeration :: [[Char]] -> [Char]
enumeration ns                 = parens (hcat (intersperse (text "|") (map nmtoken ns)))
defaultdecl :: DefaultDecl -> [Char]
defaultdecl  REQUIRED          = text "#REQUIRED"
defaultdecl  IMPLIED           = text "#IMPLIED"
defaultdecl (DefaultTo a f)    = maybe (const (text "#FIXED")) f <+> attvalue a
-- _ (IncludeSect i)= text "<![INCLUDE [" <+>
--                                  vcat (map extsubsetdecl i) <+> text "]]>"
-- conditionalsect (IgnoreSect i) = text "<![IGNORE [" <+>
--                                  fsep (map ignoresectcontents i) <+> text "]]>"
-- _ (Ignore)                = empty
-- _ (IgnoreSectContents i is)
--                                = ignore i <+> vcat (map internal is)
--                           where internal (ics,i) = text "<![[" <+>
--                                                    ignoresectcontents ics <+>
--                                                    text "]]>" <+> ignore i
reference :: Reference -> [Char]
reference (RefEntity er)       = entityref er
reference (RefChar cr)         = charref cr
entityref :: [Char] -> [Char]
entityref n                    = text "&" <> text n <> text ";"
charref :: (Show a) => a -> [Char]
charref c                      = text "&#" <> text (show c) <> text ";"
entitydecl :: EntityDecl -> [Char]
entitydecl (EntityGEDecl d)    = gedecl d
entitydecl (EntityPEDecl d)    = pedecl d
gedecl :: GEDecl -> [Char]
gedecl (GEDecl n ed)           = text "<!ENTITY" <+> text n <+> entitydef ed <>
                                 text ">"
pedecl :: PEDecl -> [Char]
pedecl (PEDecl n pd)           = text "<!ENTITY %" <> text n <+> pedef pd <>
                                 text ">"
entitydef :: EntityDef -> [Char]
entitydef (DefEntityValue ew)  = entityvalue ew
entitydef (DefExternalID i nd) = externalid i <+> maybe ndatadecl nd
pedef :: PEDef -> [Char]
pedef (PEDefEntityValue ew)    = entityvalue ew
pedef (PEDefExternalID eid)    = externalid eid
externalid :: ExternalID -> [Char]
externalid (SYSTEM sl)         = text "SYSTEM" <+> systemliteral sl
externalid (PUBLIC i sl)       = text "PUBLIC" <+> pubidliteral i <+>
                                 systemliteral sl
ndatadecl :: NDataDecl -> [Char]
ndatadecl (NDATA n)            = text "NDATA" <+> text n
-- _ (TextDecl vi ed)      = text "<?xml" <+> maybe text vi <+>
--                                  encodingdecl ed <> text "?>"
-- _ (ExtParsedEnt t c)= maybe textdecl t <+> content c
-- _ (ExtPE t esd)            = maybe textdecl t <+>
--                                  vcat (map extsubsetdecl esd)
notationdecl :: NotationDecl -> [Char]
notationdecl (NOTATION n e)    = text "<!NOTATION" <+> text n <+>
                                 either externalid publicid e <>
                                 text ">"
publicid :: PublicID -> [Char]
publicid (PUBLICID p)          = text "PUBLICID" <+> pubidliteral p
encodingdecl :: EncodingDecl -> [Char]
encodingdecl (EncodingDecl s)  = text "encoding='" <> text s <> text "'"
nmtoken :: t -> t
nmtoken s                      = text s
attvalue :: AttValue -> [Char]
attvalue (AttValue esr)        = text "\"" <>
                                 hcat (map (either text reference) esr) <>
                                 text "\""
entityvalue :: EntityValue -> [Char]
entityvalue (EntityValue evs)
  | containsDoubleQuote evs    = text "'"  <> hcat (map ev evs) <> text "'"
  | otherwise                  = text "\"" <> hcat (map ev evs) <> text "\""
ev :: EV -> String
ev (EVString s)                = text s
--ev (EVPERef p e)               = peref p
ev (EVRef r)                   = reference r
pubidliteral :: PubidLiteral -> [Char]
pubidliteral (PubidLiteral s)
    | '"' `elem` s             = text "'" <> text s <> text "'"
    | otherwise                = text "\"" <> text s <> text "\""
systemliteral :: SystemLiteral -> [Char]
systemliteral (SystemLiteral s)
    | '"' `elem` s             = text "'" <> text s <> text "'"
    | otherwise                = text "\"" <> text s <> text "\""
chardata :: t -> t
chardata s                     = {-if all isSpace s then empty else-} text s
cdsect :: [Char] -> [Char]
cdsect c                       = text "<![CDATA[" <> chardata c <> text "]]>"

qname n                        = text (printableName n)

----
containsDoubleQuote :: [EV] -> Bool
containsDoubleQuote evs = any csq evs
    where csq (EVString s) = '"' `elem` s
          csq _            = False

