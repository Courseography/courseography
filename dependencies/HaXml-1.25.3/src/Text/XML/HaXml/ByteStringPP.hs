-- | This is a fast non-pretty-printer for turning the internal representation
--   of generic structured XML documents into Lazy ByteStrings.
--   Like in Text.Xml.HaXml.Pretty, there is one pp function for each type in
--   Text.Xml.HaXml.Types, so you can pretty-print as much or as little
--   of the document as you wish.

module Text.XML.HaXml.ByteStringPP
  (
  -- * Pretty-print a whole document
    document
  -- ** Just one content
  ,   content
  -- ** Just one tagged element
  ,   element
  -- * Pretty-print just a DTD
  , doctypedecl
  -- ** The prolog
  ,   prolog
  -- ** A content particle description
  ,   cp
  ) where

import Prelude hiding (maybe,either,elem,concat)
import Data.Maybe hiding (maybe)
import Data.List (intersperse)
--import Data.ByteString.Lazy hiding (pack,map,head,any,singleton,intersperse,join)
import Data.ByteString.Lazy.Char8 (ByteString(), concat, pack, singleton
                                  , intercalate, append, elem, empty)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces

either :: (t -> t1) -> (t2 -> t1) -> Either t t2 -> t1
either f _ (Left x)  = f x
either _ g (Right x) = g x

maybe :: (t -> ByteString) -> Maybe t -> ByteString
maybe _ Nothing  = empty
maybe f (Just x) = f x


-- A simple implementation of the pretty-printing combinator interface,
-- but for plain ByteStrings:
infixl 6 <>
infixl 6 <+>
infixl 5 $$
(<>)   :: ByteString   -> ByteString -> ByteString -- Beside
hcat   :: [ByteString] -> ByteString               -- List version of <>
(<+>)  :: ByteString   -> ByteString -> ByteString -- Beside, separated by space
hsep   :: [ByteString] -> ByteString               -- List version of <+>
($$)   :: ByteString   -> ByteString -> ByteString -- Above; if there is no
                                                   -- overlap, it "dovetails"
vcat   :: [ByteString] -> ByteString       -- List version of $$
-- cat    :: [ByteString] -> ByteString    -- Either hcat or vcat
sep    :: [ByteString] -> ByteString       -- Either hsep or vcat
-- fcat   :: [ByteString] -> ByteString    -- ``Paragraph fill'' version of cat
fsep   :: [ByteString] -> ByteString       -- ``Paragraph fill'' version of sep
nest   :: Int -> ByteString -> ByteString  -- Nested

(<>)  b1 b2  = b1 `append` b2
(<+>) b1 b2  = b1 <> pack " " <> b2
($$)  b1 b2  = b1 <> pack "\n" <> b2
-- ($+$)        = ($$)

hcat = Data.ByteString.Lazy.Char8.concat
hsep = Data.ByteString.Lazy.Char8.intercalate (singleton ' ')
vcat = Data.ByteString.Lazy.Char8.intercalate (singleton '\n')
-- cat  = hcat
sep  = hsep
text :: [Char] -> ByteString
text = pack
-- fsep = cat
fsep = sep
nest _ b = pack " " <> b
parens :: ByteString -> ByteString
parens p = pack "(" <> p <> pack ")"


----
-- Now for the XML pretty-printing interface.
-- (Basically copied direct from Text.XML.HaXml.Pretty).

document :: Document i -> ByteString
prolog   :: Prolog     -> ByteString
xmldecl  :: XMLDecl    -> ByteString
misc     :: Misc       -> ByteString
sddecl   :: Bool       -> ByteString

doctypedecl   :: DocTypeDecl   -> ByteString
markupdecl    :: MarkupDecl    -> ByteString
-- extsubset     :: ExtSubset     -> ByteString
-- extsubsetdecl :: ExtSubsetDecl -> ByteString
cp            :: CP            -> ByteString

element   :: Element i -> ByteString
attribute :: Attribute -> ByteString
content   :: Content i -> ByteString

----

document (Document p _ e m)= prolog p $$ element e $$ vcat (Prelude.map misc m)
prolog (Prolog x m1 dtd m2)= maybe xmldecl x $$
                             vcat (Prelude.map misc m1) $$
                             maybe doctypedecl dtd $$
                             vcat (Prelude.map misc m2)
xmldecl (XMLDecl v e sd)   = text "<?xml version='" <> text v <> text "'" <+>
                             maybe encodingdecl e <+>
                             maybe sddecl sd <+>
                             text "?>"
misc (Comment s)           = text "<!--" <+> text s <+> text "-->"
misc (PI (n,s))            = text "<?" <> text n <+> text s <+> text "?>"
sddecl sd   | sd           = text "standalone='yes'"
            | otherwise    = text "standalone='no'"
doctypedecl (DTD n eid ds) = if Prelude.null ds then
                                  hd <> text ">"
                             else hd <+> text " [" $$
                                  vcat (Prelude.map markupdecl ds) $$ text "]>"
                           where hd = text "<!DOCTYPE" <+> qname n <+>
                                      maybe externalid eid
markupdecl (Element e)     = elementdecl e
markupdecl (AttList a)     = attlistdecl a
markupdecl (Entity e)      = entitydecl e
markupdecl (Notation n)    = notationdecl n
markupdecl (MarkupMisc m)  = misc m
--markupdecl (MarkupPE p m)  = peref p
-- _ (ExtSubset t ds) = maybe textdecl t $$
--                             vcat (Prelude.map extsubsetdecl ds)
-- _ (ExtMarkupDecl m)      = markupdecl m
-- extsubsetdecl (ExtConditionalSect c) = conditionalsect c
--extsubsetdecl (ExtPEReference p e)   = peref p

element (Elem n as []) = text "<" <> qname n <+>
                         fsep (Prelude.map attribute as) <> text "/>"
element e@(Elem n as cs)
--  | any isText cs    = text "<" <> text n <+> fsep (map attribute as) <>
--                       text ">" <> hcat (map content cs) <>
--                       text "</" <> qname n <> text ">"
    | isText (head cs) = text "<" <> qname n <+> fsep (Prelude.map attribute as) <>
                         text ">" <> hcat (Prelude.map content cs) <>
                         text "</" <> qname n <> text ">"
    | otherwise        = let (d,c) = carryelem e empty
                         in d <> c

isText :: Content t -> Bool
isText (CString _ _ _) = True
isText (CRef _ _)      = True
isText _               = False

carryelem :: Element t -> ByteString -> (ByteString, ByteString)
carryelem (Elem n as []) c
                       = ( c <>
                           text "<" <> qname n <+> fsep (Prelude.map attribute as)
                         , text "/>")
carryelem (Elem n as cs) c
--  | any isText cs    =  ( c <> element e, empty)
    | otherwise        =  let (cs0,d0) = carryscan carrycontent cs (text ">")
                          in
                          ( c <>
                            text "<" <> qname n <+> fsep (Prelude.map attribute as) $$
                            nest 2 (vcat cs0) <> --- $$
                            d0 <> text "</" <> qname n
                          , text ">")
carrycontent :: Content t -> ByteString -> (ByteString, ByteString)
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


attribute (n,v)             = text (printableName n) <> text "=" <> attvalue v
content (CElem e _)         = element e
content (CString False s _) = chardata s
content (CString True s _)  = cdsect s
content (CRef r _)          = reference r
content (CMisc m _)         = misc m

elementdecl :: ElementDecl -> ByteString
elementdecl (ElementDecl n cs) = text "<!ELEMENT" <+> qname n <+>
                                 contentspec cs <> text ">"
contentspec :: ContentSpec -> ByteString
contentspec EMPTY              = text "EMPTY"
contentspec ANY                = text "ANY"
contentspec (Mixed m)          = mixed m
contentspec (ContentSpec c)    = cp c
--contentspec (ContentPE p cs)   = peref p
cp (TagName n m)       = qname n <> modifier m
cp (Choice cs m)       = parens (hcat (intersperse (text "|") (Prelude.map cp cs))) <>
                           modifier m
cp (Seq cs m)          = parens (hcat (intersperse (text ",") (Prelude.map cp cs))) <>
                           modifier m
--cp (CPPE p c)          = peref p
modifier :: Modifier -> ByteString
modifier None          = empty
modifier Query         = text "?"
modifier Star          = text "*"
modifier Plus          = text "+"
mixed :: Mixed -> ByteString
mixed  PCDATA          = text "(#PCDATA)"
mixed (PCDATAplus ns)  = text "(#PCDATA |" <+>
                         hcat (intersperse (text "|") (Prelude.map qname ns)) <>
                         text ")*"

attlistdecl :: AttListDecl -> ByteString
attlistdecl (AttListDecl n ds) = text "<!ATTLIST" <+> qname n <+>
                                 fsep (Prelude.map attdef ds) <> text ">"
attdef :: AttDef -> ByteString
attdef (AttDef n t d)          = qname n <+> atttype t <+> defaultdecl d
atttype :: AttType -> ByteString
atttype  StringType            = text "CDATA"
atttype (TokenizedType t)      = tokenizedtype t
atttype (EnumeratedType t)     = enumeratedtype t
tokenizedtype :: TokenizedType -> ByteString
tokenizedtype ID               = text "ID"
tokenizedtype IDREF            = text "IDREF"
tokenizedtype IDREFS           = text "IDREFS"
tokenizedtype ENTITY           = text "ENTITY"
tokenizedtype ENTITIES         = text "ENTITIES"
tokenizedtype NMTOKEN          = text "NMTOKEN"
tokenizedtype NMTOKENS         = text "NMTOKENS"
enumeratedtype :: EnumeratedType -> ByteString
enumeratedtype (NotationType n)= notationtype n
enumeratedtype (Enumeration e) = enumeration e
notationtype :: [[Char]] -> ByteString
notationtype ns                = text "NOTATION" <+>
                                 parens (hcat (intersperse (text "|") (Prelude.map text ns)))
enumeration :: [[Char]] -> ByteString
enumeration ns                 = parens (hcat (intersperse (text "|") (Prelude.map nmtoken ns)))
defaultdecl :: DefaultDecl -> ByteString
defaultdecl  REQUIRED          = text "#REQUIRED"
defaultdecl  IMPLIED           = text "#IMPLIED"
defaultdecl (DefaultTo a f)    = maybe (const (text "#FIXED")) f <+> attvalue a
--_ (IncludeSect i)= text "<![INCLUDE [" <+>
--                                 vcat (Prelude.map extsubsetdecl i) <+> text "]]>"
-- conditionalsect (IgnoreSect i) = text "<![IGNORE [" <+>
--                                  fsep (Prelude.map ignoresectcontents i) <+> text "]]>"
-- -- _ (Ignore)                = empty
-- ignoresectcontents :: IgnoreSectContents -> ByteString
-- _ (IgnoreSectContents i is)
--                                = ignore i <+> vcat (Prelude.map internal is)
--                           where internal (ics,i) = text "<![[" <+>
--                                                    ignoresectcontents ics <+>
--                                                    text "]]>" <+> ignore i
reference :: Reference -> ByteString
reference (RefEntity er)       = entityref er
reference (RefChar cr)         = charref cr
entityref :: [Char] -> ByteString
entityref n                    = text "&" <> text n <> text ";"
charref :: (Show a) => a -> ByteString
charref c                      = text "&#" <> text (show c) <> text ";"
entitydecl :: EntityDecl -> ByteString
entitydecl (EntityGEDecl d)    = gedecl d
entitydecl (EntityPEDecl d)    = pedecl d
gedecl :: GEDecl -> ByteString
gedecl (GEDecl n ed)           = text "<!ENTITY" <+> text n <+> entitydef ed <>
                                 text ">"
pedecl :: PEDecl -> ByteString
pedecl (PEDecl n pd)           = text "<!ENTITY %" <> text n <+> pedef pd <>
                                 text ">"
entitydef :: EntityDef -> ByteString
entitydef (DefEntityValue ew)  = entityvalue ew
entitydef (DefExternalID i nd) = externalid i <+> maybe ndatadecl nd
pedef :: PEDef -> ByteString
pedef (PEDefEntityValue ew)    = entityvalue ew
pedef (PEDefExternalID eid)    = externalid eid
externalid :: ExternalID -> ByteString
externalid (SYSTEM sl)         = text "SYSTEM" <+> systemliteral sl
externalid (PUBLIC i sl)       = text "PUBLIC" <+> pubidliteral i <+>
                                 systemliteral sl
ndatadecl :: NDataDecl -> ByteString
ndatadecl (NDATA n)            = text "NDATA" <+> text n
-- _ (TextDecl vi ed)      = text "<?xml" <+> maybe text vi <+>
--                                  encodingdecl ed <> text "?>"
-- extparsedent :: ExtParsedEnt t -> ByteString
-- _ (ExtParsedEnt t c)= maybe textdecl t <+> content c
-- _ (ExtPE t esd)            = maybe textdecl t <+>
--                                  vcat (Prelude.map extsubsetdecl esd)
notationdecl :: NotationDecl -> ByteString
notationdecl (NOTATION n e)    = text "<!NOTATION" <+> text n <+>
                                 either externalid publicid e <>
                                 text ">"
publicid :: PublicID -> ByteString
publicid (PUBLICID p)          = text "PUBLICID" <+> pubidliteral p
encodingdecl :: EncodingDecl -> ByteString
encodingdecl (EncodingDecl s)  = text "encoding='" <> text s <> text "'"
nmtoken :: [Char] -> ByteString
nmtoken s                      = text s
attvalue :: AttValue -> ByteString
attvalue (AttValue esr)        = text "\"" <>
                                 hcat (Prelude.map (either text reference) esr) <>
                                 text "\""
entityvalue :: EntityValue -> ByteString
entityvalue (EntityValue evs)
  | containsDoubleQuote evs    = text "'"  <> hcat (Prelude.map ev evs) <> text "'"
  | otherwise                  = text "\"" <> hcat (Prelude.map ev evs) <> text "\""
ev :: EV -> ByteString
ev (EVString s)                = text s
--ev (EVPERef p e)               = peref p
ev (EVRef r)                   = reference r
pubidliteral :: PubidLiteral -> ByteString
pubidliteral (PubidLiteral s)
    | toWord8 '"' `elem` (pack s) = text "'" <> text s <> text "'"
    | otherwise                = text "\"" <> text s <> text "\""
systemliteral :: SystemLiteral -> ByteString
systemliteral (SystemLiteral s)
    | toWord8 '"' `elem` (pack s) = text "'" <> text s <> text "'"
    | otherwise                = text "\"" <> text s <> text "\""
chardata, cdsect :: [Char] -> ByteString
chardata s                     = {-if all isSpace s then empty else-} text s
cdsect c                       = text "<![CDATA[" <> chardata c <> text "]]>"

qname n                        = text (printableName n)

-- toWord8 :: Char -> Word8
toWord8 :: (Enum a, Enum a1) => a1 -> a
toWord8 = toEnum . fromEnum

containsDoubleQuote :: [EV] -> Bool
containsDoubleQuote evs = any csq evs
    where csq (EVString s) = toWord8 '"' `elem` (pack s)
          csq _            = False
