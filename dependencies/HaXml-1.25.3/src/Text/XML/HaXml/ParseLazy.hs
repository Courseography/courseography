-- | A non-validating XML parser.  For the input grammar, see
--   <http://www.w3.org/TR/REC-xml>.
module Text.XML.HaXml.ParseLazy
  (
  -- * Parse a whole document
    xmlParse  -- , xmlParse'
  -- * Parse just a DTD
  , dtdParse  -- , dtdParse'
  -- * Parse a partial document
  , xmlParseWith
  -- * Individual parsers for use with /xmlParseWith/ and module SAX
  , document, element, content
  , comment, chardata
  , reference, doctypedecl
  , processinginstruction
  , elemtag, qname, name, tok
  , elemOpenTag, elemCloseTag
  , emptySTs, XParser
  -- * These general utility functions don't belong here
  , fst3, snd3, thd3
  ) where

-- An XML parser, written using a slightly extended version of the
-- Hutton/Meijer parser combinators.  The input is tokenised internally
-- by the lexer xmlLex.  Whilst parsing, we gather a symbol
-- table of entity references.  PERefs must be defined before use, so we
-- expand their uses as we encounter them, forcing the remainder of the
-- input to be re-lexed and re-parsed.  GERefs are simply stored for
-- later retrieval.

import Prelude hiding (either,maybe,sequence,catch)
import qualified Prelude (either)
import Data.Maybe hiding (maybe)
import Data.List (intersperse)       -- debugging only
import Data.Char (isSpace,isDigit,isHexDigit)
import Control.Monad hiding (sequence)
import Numeric (readDec,readHex)
--import Control.Exception (catch)

import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Lex
import Text.ParserCombinators.Poly.StateLazy

import System.FilePath (combine, dropFileName)


#if ( defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 502 ) || \
    ( defined(__NHC__) && __NHC__ > 114 ) || defined(__HUGS__)
import System.IO.Unsafe (unsafePerformIO)
#elif defined(__GLASGOW_HASKELL__)
import IOExts (unsafePerformIO)
#elif defined(__NHC__)
import IOExtras (unsafePerformIO)
#elif defined(__HBC__)
import UnsafePerformIO
#endif

--  #define DEBUG

#if defined(DEBUG)
#  if ( defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 502 ) || \
      ( defined(__NHC__) && __NHC__ > 114 ) || defined(__HUGS__)
import Debug.Trace(trace)
#  elif defined(__GLASGOW_HASKELL__)
import IOExts(trace)
#  elif defined(__NHC__) || defined(__HBC__)
import NonStdTrace
#  endif
v `debug` s = trace s v
#else
v `debug` _ = v
#endif
debug :: a -> String -> a


-- | To parse a whole document, @xmlParse file content@ takes a filename
--   (for generating error reports) and the string content of that file.
--   A parse error causes program failure, with message to stderr.
xmlParse :: String -> String -> Document Posn

{-
-- | To parse a whole document, @xmlParse' file content@ takes a filename
--   (for generating error reports) and the string content of that file.
--   Any parse error message is passed back to the caller through the
--   @Either@ type.
xmlParse' :: String -> String -> Either String (Document Posn)
-}

-- | To parse just a DTD, @dtdParse file content@ takes a filename
--   (for generating error reports) and the string content of that
--   file.  If no DTD was found, you get @Nothing@ rather than an error.
--   However, if a DTD is found but contains errors, the program crashes.
dtdParse  :: String -> String -> Maybe DocTypeDecl

{-
-- | To parse just a DTD, @dtdParse' file content@ takes a filename
--   (for generating error reports) and the string content of that
--   file.  If no DTD was found, you get @Right Nothing@.
--   If a DTD was found but contains errors, you get a @Left message@.
dtdParse' :: String -> String -> Either String (Maybe DocTypeDecl)

xmlParse' name inp = xmlParse name inp `catch` (Left . show)
dtdParse' name inp = dtdParse name inp `catch` (Left . show)
-}

xmlParse  name  = fst3 . runParser (toEOF document) emptySTs . xmlLex name
dtdParse  name  = fst3 . runParser justDTD  emptySTs . xmlLex name

toEOF :: XParser a -> XParser a
toEOF = id	-- there are other possible implementations...

-- | To parse a partial document, e.g. from an XML-based stream protocol,
--   where you may later want to get more document elements from the same
--   stream.  Arguments are: a parser for the item you want, and the
--   already-lexed input to parse from.  Returns the item you wanted
--   (or an error message), plus the remainder of the input.
xmlParseWith :: XParser a -> [(Posn,TokenT)]
                -> (Either String a, [(Posn,TokenT)])
xmlParseWith p = (\(v,_,s)->(Right v,s)) . runParser p emptySTs


---- Symbol table stuff ----

type SymTabs = (SymTab PEDef, SymTab EntityDef)

-- | Some empty symbol tables for GE and PE references.
emptySTs :: SymTabs
emptySTs = (emptyST, emptyST)

addPE :: String -> PEDef -> SymTabs -> SymTabs
addPE n v (pe,ge) = (addST n v pe, ge)

addGE :: String -> EntityDef -> SymTabs -> SymTabs
addGE n v (pe,ge) = let newge = addST n v ge in newge `seq` (pe, newge)

lookupPE :: String -> SymTabs -> Maybe PEDef
lookupPE s (pe,_ge) = lookupST s pe

flattenEV :: EntityValue -> String
flattenEV (EntityValue evs) = concatMap flatten evs
  where
    flatten (EVString s)          = s
    flatten (EVRef (RefEntity r)) = "&" ++r++";"
    flatten (EVRef (RefChar r))   = "&#"++show r++";"
 -- flatten (EVPERef n)           = "%" ++n++";"


---- Misc ----
fst3 :: (a,b,c) -> a
snd3 :: (a,b,c) -> b
thd3 :: (a,b,c) -> c

fst3 (a,_,_) = a
snd3 (_,a,_) = a
thd3 (_,_,a) = a


---- Auxiliary Parsing Functions ----

-- | XParser is just a specialisation of the PolyStateLazy parser.
type XParser a = Parser SymTabs (Posn,TokenT) a

-- | Return the next token from the input only if it matches the given token.
tok :: TokenT -> XParser TokenT
tok t = do (p,t') <- next
           case t' of TokError _    -> report failBad (show t) p t'
                      _ | t'==t     -> return t
                        | otherwise -> report fail (show t) p t'
nottok :: [TokenT] -> XParser TokenT
nottok ts = do (p,t) <- next
               if t`elem`ts then report fail ("no "++show t) p t
                            else return t

-- | Return a qualified name (although the namespace qualification is not
--   processed here; this is merely to get the correct type).
qname :: XParser QName
qname = fmap N name

-- | Return just a name, e.g. element name, attribute name.
name :: XParser Name
name = do (p,tok) <- next
          case tok of
            TokName s  -> return s
            TokError _ -> report failBad "a name" p tok
            _          -> report fail "a name" p tok

string, freetext :: XParser String
string   = do (p,t) <- next
              case t of TokName s -> return s
                        _         -> report fail "text" p t
freetext = do (p,t) <- next
              case t of TokFreeText s -> return s
                        _             -> report fail "text" p t

maybe :: XParser a -> XParser (Maybe a)
maybe p =
    ( p >>= return . Just) `onFail`
    ( return Nothing)

either :: XParser a -> XParser b -> XParser (Either a b)
either p q =
    ( p >>= return . Left) `onFail`
    ( q >>= return . Right)

word :: String -> XParser ()
word s = do { x <- next
            ; case x of
                (_p,TokName n)     | s==n -> return ()
                (_p,TokFreeText n) | s==n -> return ()
                ( p,t@(TokError _)) -> report failBad (show s) p t
                ( p,t) -> report fail (show s) p t
            }

posn :: XParser Posn
posn = do { x@(p,_) <- next
          ; reparse [x]
          ; return p
          }

nmtoken :: XParser NmToken
nmtoken = (string `onFail` freetext)

failP, failBadP :: String -> XParser a
failP msg = do { p <- posn; fail (msg++"\n    at "++show p) }
failBadP msg = do { p <- posn; failBad (msg++"\n    at "++show p) }

report :: (String->XParser a) -> String -> Posn -> TokenT -> XParser a
report fail expect p t = fail ("Expected "++expect++" but found "++show t
                               ++"\n  in "++show p)

adjustErrP :: XParser a -> (String->String) -> XParser a
p `adjustErrP` f = p `onFail` do pn <- posn
                                 (p `adjustErr` f) `adjustErr` (++show pn)

peRef :: XParser a -> XParser a
peRef p =
    p `onFail`
    do pn <- posn
       n <- pereference
       tr <- stQuery (lookupPE n) `debug` ("Looking up %"++n)
       case tr of
           Just (PEDefEntityValue ev) ->
                      do reparse (xmlReLex (posInNewCxt ("macro %"++n++";")
                                                        (Just pn))
                                           (flattenEV ev))
                               `debug` ("  defn:  "++flattenEV ev)
                         peRef p
           Just (PEDefExternalID (PUBLIC _ (SystemLiteral f))) ->
                      do let f' = combine (dropFileName $ posnFilename pn) f
                             val = unsafePerformIO (readFile f')
                         reparse (xmlReLex (posInNewCxt f'
                                                        (Just pn)) val)
                               `debug` ("  reading from file "++f')
                         peRef p
           Just (PEDefExternalID (SYSTEM (SystemLiteral f))) ->
                      do let f' = combine (dropFileName $ posnFilename pn) f
                             val = unsafePerformIO (readFile f')
                         reparse (xmlReLex (posInNewCxt f'
                                                        (Just pn)) val)
                               `debug` ("  reading from file "++f')
                         peRef p
           Nothing -> fail ("PEReference use before definition: "++"%"++n++";"
                           ++"\n    at "++show pn)

blank :: XParser a -> XParser a
blank p =
    p `onFail`
    do n <- pereference
       tr <- stQuery (lookupPE n) `debug` ("Looking up %"++n++" (is blank?)")
       case tr of
           Just (PEDefEntityValue ev)
                    | all isSpace (flattenEV ev)  ->
                            do blank p `debug` "Empty macro definition"
           Just _  -> failP ("expected a blank PERef macro: "++"%"++n++";")
           Nothing -> failP ("PEReference use before definition: "++"%"++n++";")



---- XML Parsing Functions ----

justDTD :: XParser (Maybe DocTypeDecl)
justDTD =
  do (ExtSubset _ ds) <- extsubset `debug` "Trying external subset"
     if null ds then fail "empty"
         else return (Just (DTD (N "extsubset") Nothing (concatMap extract ds)))
  `onFail`
  do (Prolog _ _ dtd _) <- prolog
     return dtd
 where extract (ExtMarkupDecl m) = [m]
       extract (ExtConditionalSect (IncludeSect i)) = concatMap extract i
       extract (ExtConditionalSect (IgnoreSect _)) = []

-- | Return an entire XML document including prolog and trailing junk.
document :: XParser (Document Posn)
document = do
 -- p <- prolog `adjustErr` ("unrecognisable XML prolog\n"++)
 -- e <- element
 -- ms <- many misc
 -- (_,ge) <- stGet
 -- return (Document p ge e ms)
    return Document `apply` (prolog `adjustErr`
                                    ("unrecognisable XML prolog\n"++))
                    `apply` (fmap snd stGet)
                    `apply` element
                    `apply` many misc

-- | Return an XML comment.
comment :: XParser Comment
comment = do
    bracket (tok TokCommentOpen) (tok TokCommentClose) freetext
--  tok TokCommentOpen
--  commit $ do
--    c <- freetext
--    tok TokCommentClose
--    return c

-- | Parse a processing instruction.
processinginstruction :: XParser ProcessingInstruction
processinginstruction = do
    tok TokPIOpen
    commit $ do
      n <- string  `onFail` failP "processing instruction has no target"
      f <- freetext
      tok TokPIClose `onFail` failP ("missing ?> in <?"++n)
      return (n, f)

cdsect :: XParser CDSect
cdsect = do
    tok TokSectionOpen
    bracket (tok (TokSection CDATAx)) (commit $ tok TokSectionClose) chardata

prolog :: XParser Prolog
prolog = do
    x   <- maybe xmldecl
    m1  <- many misc
    dtd <- maybe doctypedecl
    m2  <- many misc
    return (Prolog x m1 dtd m2)

xmldecl :: XParser XMLDecl
xmldecl = do
    tok TokPIOpen
    (word "xml" `onFail` word "XML")
    p <- posn
    s <- freetext
    tok TokPIClose `onFail` failBadP "missing ?> in <?xml ...?>"
 -- raise ((runParser aux emptySTs . xmlReLex p) s)
    return (fst3 ((runParser aux emptySTs . xmlReLex p) s))
  where
    aux = do
        v <- versioninfo  `onFail` failP "missing XML version info"
        e <- maybe encodingdecl
        s <- maybe sddecl
        return (XMLDecl v e s)
 -- raise (Left err, _, _) = failP err
 -- raise (Right ok, _, _) = return ok

versioninfo :: XParser VersionInfo
versioninfo = do
    (word "version" `onFail` word "VERSION")
    tok TokEqual
    bracket (tok TokQuote) (commit $ tok TokQuote) freetext

misc :: XParser Misc
misc =
    oneOf' [ ("<!--comment-->",  comment >>= return . Comment)
           , ("<?PI?>",          processinginstruction >>= return . PI)
           ]

-- | Return a DOCTYPE decl, indicating a DTD.
doctypedecl :: XParser DocTypeDecl
doctypedecl = do
    tok TokSpecialOpen
    tok (TokSpecial DOCTYPEx)
    commit $ do
      n   <- qname
      eid <- maybe externalid
      es  <- maybe (bracket (tok TokSqOpen) (commit $ tok TokSqClose)
                            (many (peRef markupdecl)))
      blank (tok TokAnyClose)  `onFail` failP "missing > in DOCTYPE decl"
      return (DTD n eid (case es of { Nothing -> []; Just e -> e }))

-- | Return a DTD markup decl, e.g. ELEMENT, ATTLIST, etc
markupdecl :: XParser MarkupDecl
markupdecl =
  oneOf' [ ("ELEMENT",  elementdecl  >>= return . Element)
         , ("ATTLIST",  attlistdecl  >>= return . AttList)
         , ("ENTITY",   entitydecl   >>= return . Entity)
         , ("NOTATION", notationdecl >>= return . Notation)
         , ("misc",     misc         >>= return . MarkupMisc)
         ]
    `adjustErrP`
          ("when looking for a markup decl,\n"++)
 --       (\  (ELEMENT, ATTLIST, ENTITY, NOTATION, <!--comment-->, or <?PI?>")

extsubset :: XParser ExtSubset
extsubset = do
    td <- maybe textdecl
    ds <- many (peRef extsubsetdecl)
    return (ExtSubset td ds)

extsubsetdecl :: XParser ExtSubsetDecl
extsubsetdecl =
    ( markupdecl >>= return . ExtMarkupDecl) `onFail`
    ( conditionalsect >>= return . ExtConditionalSect)

sddecl :: XParser SDDecl
sddecl = do
    (word "standalone" `onFail` word "STANDALONE")
    commit $ do
      tok TokEqual `onFail` failP "missing = in 'standalone' decl"
      bracket (tok TokQuote) (commit $ tok TokQuote)
              ( (word "yes" >> return True) `onFail`
                (word "no" >> return False) `onFail`
                failP "'standalone' decl requires 'yes' or 'no' value" )

{-
element :: XParser (Element Posn)
element = do
    tok TokAnyOpen
    (ElemTag n as) <- elemtag
    oneOf' [ ("self-closing tag <"++n++"/>"
             ,  do tok TokEndClose
                   return (Elem n as []))
           , ("after open tag <"++n++">"
             ,  do tok TokAnyClose
                   cs <- many content
                   p  <- posn
                   m  <- bracket (tok TokEndOpen) (commit $ tok TokAnyClose) qname
                   checkmatch p n m
                   return (Elem n as cs))
           ] `adjustErr` (("in element tag "++n++",\n")++)
-}

-- | Return a complete element including all its inner content.
element :: XParser (Element Posn)
element = do
    tok TokAnyOpen
    (ElemTag n as) <- elemtag
    return (Elem n as) `apply`
        ( do tok TokEndClose
             return []
          `onFail`
          do tok TokAnyClose
             commit $ manyFinally content
                                  (do p <- posn
                                      m <- bracket (tok TokEndOpen)
                                                   (commit $ tok TokAnyClose) qname
                                      checkmatch p n m)
          ) `adjustErrBad` (("in element tag "++printableName n++",\n")++)

checkmatch :: Posn -> QName -> QName -> XParser ()
checkmatch p n m =
  if n == m then return ()
  else failBad ("tag <"++printableName n++"> terminated by </"++printableName m
                ++">\n  at "++show p)

-- | Parse only the parts between angle brackets in an element tag.
elemtag :: XParser ElemTag
elemtag = do
    n  <- qname `adjustErrBad` ("malformed element tag\n"++)
    as <- many attribute
    return (ElemTag n as)

-- | For use with stream parsers - returns the complete opening element tag.
elemOpenTag :: XParser ElemTag
elemOpenTag = do
    tok TokAnyOpen
    e <- elemtag
    tok TokAnyClose
    return e

-- | For use with stream parsers - accepts a closing tag, provided it
--   matches the given element name.
elemCloseTag :: QName -> XParser ()
elemCloseTag n = do
    tok TokEndOpen
    p <- posn
    m <- qname
    tok TokAnyClose
    checkmatch p n m

attribute :: XParser Attribute
attribute = do
    n <- qname `adjustErr` ("malformed attribute name\n"++)
    tok TokEqual `onFail` failBadP "missing = in attribute"
    v <- attvalue `onFail` failBadP "missing attvalue"
    return (n,v)

-- | Return a content particle, e.g. text, element, reference, etc
content :: XParser (Content Posn)
content =
  do { p  <- posn
     ; c' <- content'
     ; return (c' p)
     }
  where
     content' = oneOf' [ ("element",   element   >>= return . CElem)
                       , ("chardata",  chardata  >>= return . CString False)
                       , ("reference", reference >>= return . CRef)
                       , ("CDATA",     cdsect    >>= return . CString True)
                       , ("misc",      misc      >>= return . CMisc)
                       ]
                  `adjustErrP` ("when looking for a content item,\n"++)
-- (\    (element, text, reference, CDATA section, <!--comment-->, or <?PI?>")

elementdecl :: XParser ElementDecl
elementdecl = do
    tok TokSpecialOpen
    tok (TokSpecial ELEMENTx)
    n <- peRef qname `adjustErrBad` ("expecting identifier in ELEMENT decl\n"++)
    c <- peRef contentspec
             `adjustErrBad` (("in content spec of ELEMENT decl: "
                             ++printableName n++"\n")++)
    blank (tok TokAnyClose) `onFail` failBadP
       ("expected > terminating ELEMENT decl"
       ++"\n    element name was "++show (printableName n)
       ++"\n    contentspec was "++(\ (ContentSpec p)-> debugShowCP p) c)
    return (ElementDecl n c)

contentspec :: XParser ContentSpec
contentspec =
    oneOf' [ ("EMPTY",  peRef (word "EMPTY") >> return EMPTY)
           , ("ANY",    peRef (word "ANY") >> return ANY)
           , ("mixed",  peRef mixed >>= return . Mixed)
           , ("simple", peRef cp >>= return . ContentSpec)
           ]
 --   `adjustErr` ("when looking for content spec,\n"++)
 --   `adjustErr` (++"\nLooking for content spec (EMPTY, ANY, mixed, etc)")

choice :: XParser [CP]
choice = do
    bracket (tok TokBraOpen `debug` "Trying choice")
            (blank (tok TokBraClose `debug` "Succeeded with choice"))
            (peRef cp `sepBy1` blank (tok TokPipe))

sequence :: XParser [CP]
sequence = do
    bracket (tok TokBraOpen `debug` "Trying sequence")
            (blank (tok TokBraClose `debug` "Succeeded with sequence"))
            (peRef cp `sepBy1` blank (tok TokComma))

cp :: XParser CP
cp = oneOf [ ( do n <- qname
                  m <- modifier
                  let c = TagName n m
                  return c `debug` ("ContentSpec: name "++debugShowCP c))
           , ( do ss <- sequence
                  m <- modifier
                  let c = Seq ss m
                  return c `debug` ("ContentSpec: sequence "++debugShowCP c))
           , ( do cs <- choice
                  m <- modifier
                  let c = Choice cs m
                  return c `debug` ("ContentSpec: choice "++debugShowCP c))
           ] `adjustErr` (++"\nwhen looking for a content particle")

modifier :: XParser Modifier
modifier = oneOf [ ( tok TokStar >> return Star )
                 , ( tok TokQuery >> return Query )
                 , ( tok TokPlus >> return Plus )
                 , ( return None )
                 ]

-- just for debugging
debugShowCP :: CP -> String
debugShowCP cp = case cp of
    TagName n m  -> printableName n++debugShowModifier m
    Choice cps m -> '(': concat (intersperse "|" (map debugShowCP cps))++")"++debugShowModifier m
    Seq cps m    -> '(': concat (intersperse "," (map debugShowCP cps))++")"++debugShowModifier m
debugShowModifier :: Modifier -> String
debugShowModifier modifier = case modifier of
    None  -> ""
    Query -> "?"
    Star  -> "*"
    Plus  -> "+"
----

mixed :: XParser Mixed
mixed = do
    tok TokBraOpen
    peRef (do tok TokHash
              word "PCDATA")
    commit $
      oneOf [ ( do cs <- many (peRef (do tok TokPipe
                                         peRef qname))
                   blank (tok TokBraClose >> tok TokStar)
                   return (PCDATAplus cs))
            , ( blank (tok TokBraClose >> tok TokStar) >> return PCDATA)
            , ( blank (tok TokBraClose) >> return PCDATA)
            ]
        `adjustErrP` (++"\nLooking for mixed content spec (#PCDATA | ...)*\n")

attlistdecl :: XParser AttListDecl
attlistdecl = do
    tok TokSpecialOpen
    tok (TokSpecial ATTLISTx)
    n <- peRef qname `adjustErrBad` ("expecting identifier in ATTLIST\n"++)
    ds <- peRef (many1 (peRef attdef))
    blank (tok TokAnyClose) `onFail` failBadP "missing > terminating ATTLIST"
    return (AttListDecl n ds)

attdef :: XParser AttDef
attdef =
  do n <- peRef qname `adjustErr` ("expecting attribute name\n"++)
     t <- peRef atttype `adjustErr` (("within attlist defn: "
                                      ++printableName n++",\n")++)
     d <- peRef defaultdecl `adjustErr` (("in attlist defn: "
                                          ++printableName n++",\n")++)
     return (AttDef n t d)

atttype :: XParser AttType
atttype =
    oneOf' [ ("CDATA",      word "CDATA" >> return StringType)
           , ("tokenized",  tokenizedtype >>= return . TokenizedType)
           , ("enumerated", enumeratedtype >>= return . EnumeratedType)
           ]
      `adjustErr` ("looking for ATTTYPE,\n"++)
 --   `adjustErr` (++"\nLooking for ATTTYPE (CDATA, tokenized, or enumerated")

tokenizedtype :: XParser TokenizedType
tokenizedtype =
    oneOf [ ( word "ID" >> return ID)
          , ( word "IDREF" >> return IDREF)
          , ( word "IDREFS" >> return IDREFS)
          , ( word "ENTITY" >> return ENTITY)
          , ( word "ENTITIES" >> return ENTITIES)
          , ( word "NMTOKEN" >> return NMTOKEN)
          , ( word "NMTOKENS" >> return NMTOKENS)
          ] `onFail`
    do { t <- next
       ; failP ("Expected one of"
               ++" (ID, IDREF, IDREFS, ENTITY, ENTITIES, NMTOKEN, NMTOKENS)"
               ++"\nbut got "++show t)
       }

enumeratedtype :: XParser EnumeratedType
enumeratedtype =
    oneOf' [ ("NOTATION",   notationtype >>= return . NotationType)
           , ("enumerated", enumeration >>= return . Enumeration)
           ]
      `adjustErr` ("looking for an enumerated or NOTATION type,\n"++)

notationtype :: XParser NotationType
notationtype = do
    word "NOTATION"
    bracket (tok TokBraOpen) (commit $ blank $ tok TokBraClose)
            (peRef name `sepBy1` peRef (tok TokPipe))

enumeration :: XParser Enumeration
enumeration =
    bracket (tok TokBraOpen) (commit $ blank $ tok TokBraClose)
            (peRef nmtoken `sepBy1` blank (peRef (tok TokPipe)))

defaultdecl :: XParser DefaultDecl
defaultdecl =
    oneOf' [ ("REQUIRED",  tok TokHash >> word "REQUIRED" >> return REQUIRED)
           , ("IMPLIED",   tok TokHash >> word "IMPLIED" >> return IMPLIED)
           , ("FIXED",     do f <- maybe (tok TokHash >> word "FIXED"
                                                      >> return FIXED)
                              a <- peRef attvalue
                              return (DefaultTo a f) )
           ]
        `adjustErr` ("looking for an attribute default decl,\n"++)

conditionalsect :: XParser ConditionalSect
conditionalsect = oneOf'
    [ ( "INCLUDE"
      , do tok TokSectionOpen
           peRef (tok (TokSection INCLUDEx))
           p <- posn
           tok TokSqOpen `onFail` failBadP "missing [ after INCLUDE"
           i <- many (peRef extsubsetdecl)
           tok TokSectionClose
                   `onFail` failBadP ("missing ]]> for INCLUDE section"
                                     ++"\n    begun at "++show p)
           return (IncludeSect i))
    , ( "IGNORE"
      , do tok TokSectionOpen
           peRef (tok (TokSection IGNOREx))
           p <- posn
           tok TokSqOpen `onFail` failBadP "missing [ after IGNORE"
           i <- many newIgnore  -- many ignoresectcontents
           tok TokSectionClose
                   `onFail` failBadP ("missing ]]> for IGNORE section"
                                     ++"\n    begun at "++show p)
           return (IgnoreSect []))
    ] `adjustErr` ("in a conditional section,\n"++)

newIgnore :: XParser Ignore
newIgnore =
    ( do tok TokSectionOpen
         many newIgnore `debug` "IGNORING conditional section"
         tok TokSectionClose
         return Ignore `debug` "end of IGNORED conditional section") `onFail`
    ( do t <- nottok [TokSectionOpen,TokSectionClose]
         return Ignore  `debug` ("ignoring: "++show t))

--- obsolete?
--ignoresectcontents :: XParser IgnoreSectContents
--ignoresectcontents = do
--    i <- ignore
--    is <- many (do tok TokSectionOpen
--                   ic <- ignoresectcontents
--                   tok TokSectionClose
--                   ig <- ignore
--                   return (ic,ig))
--    return (IgnoreSectContents i is)
--
--ignore :: XParser Ignore
--ignore = do
--  is <- many1 (nottok [TokSectionOpen,TokSectionClose])
--  return Ignore  `debug` ("ignored all of: "++show is)
----

-- | Return either a general entity reference, or a character reference.
reference :: XParser Reference
reference = do
    bracket (tok TokAmp) (commit $ tok TokSemi) (freetext >>= val)
  where
    val ('#':'x':i) | all isHexDigit i
                    = return . RefChar . fst . head . readHex $ i
    val ('#':i)     | all isDigit i
                    = return . RefChar . fst . head . readDec $ i
    val name        = return . RefEntity $ name

{- -- following is incorrect
reference =
    ( charref >>= return . RefChar) `onFail`
    ( entityref >>= return . RefEntity)

entityref :: XParser EntityRef
entityref = do
    bracket (tok TokAmp) (commit $ tok TokSemi) name

charref :: XParser CharRef
charref = do
    bracket (tok TokAmp) (commit $ tok TokSemi) (freetext >>= readCharVal)
  where
    readCharVal ('#':'x':i) = return . fst . head . readHex $ i
    readCharVal ('#':i)     = return . fst . head . readDec $ i
    readCharVal _           = mzero
-}

pereference :: XParser PEReference
pereference = do
    bracket (tok TokPercent) (tok TokSemi) nmtoken

entitydecl :: XParser EntityDecl
entitydecl =
    ( gedecl >>= return . EntityGEDecl) `onFail`
    ( pedecl >>= return . EntityPEDecl)

gedecl :: XParser GEDecl
gedecl = do
    tok TokSpecialOpen
    tok (TokSpecial ENTITYx)
    n <- name
    e <- entitydef `adjustErrBad` (("in general entity defn "++n++",\n")++)
    tok TokAnyClose `onFail` failBadP ("expected > terminating G ENTITY decl "++n)
    stUpdate (addGE n e) `debug` ("added GE defn &"++n++";")
    return (GEDecl n e)

pedecl :: XParser PEDecl
pedecl = do
    tok TokSpecialOpen
    tok (TokSpecial ENTITYx)
    tok TokPercent
    n <- name
    e <- pedef `adjustErrBad` (("in parameter entity defn "++n++",\n")++)
    tok TokAnyClose `onFail` failBadP ("expected > terminating P ENTITY decl "++n)
    stUpdate (addPE n e) `debug` ("added PE defn %"++n++";\n"++show e)
    return (PEDecl n e)

entitydef :: XParser EntityDef
entitydef =
    oneOf' [ ("entityvalue", entityvalue >>= return . DefEntityValue)
           , ("external",    do eid <- externalid
                                ndd <- maybe ndatadecl
                                return (DefExternalID eid ndd))
           ]

pedef :: XParser PEDef
pedef =
    oneOf' [ ("entityvalue", entityvalue >>= return . PEDefEntityValue)
           , ("externalid",  externalid  >>= return . PEDefExternalID)
           ]

externalid :: XParser ExternalID
externalid =
    oneOf' [ ("SYSTEM", do word "SYSTEM"
                           s <- systemliteral
                           return (SYSTEM s) )
           , ("PUBLIC", do word "PUBLIC"
                           p <- pubidliteral
                           s <- systemliteral
                           return (PUBLIC p s) )
           ]
      `adjustErr` ("looking for an external id,\n"++)

ndatadecl :: XParser NDataDecl
ndatadecl = do
    word "NDATA"
    n <- name
    return (NDATA n)

textdecl :: XParser TextDecl
textdecl = do
    tok TokPIOpen
    (word "xml" `onFail` word "XML")
    v <- maybe versioninfo
    e <- encodingdecl
    tok TokPIClose `onFail` failP "expected ?> terminating text decl"
    return (TextDecl v e)

--extparsedent :: XParser (ExtParsedEnt Posn)
--extparsedent = do
--    t <- maybe textdecl
--    c <- content
--    return (ExtParsedEnt t c)
--
--extpe :: XParser ExtPE
--extpe = do
--    t <- maybe textdecl
--    e <- many (peRef extsubsetdecl)
--    return (ExtPE t e)

encodingdecl :: XParser EncodingDecl
encodingdecl = do
    (word "encoding" `onFail` word "ENCODING")
    tok TokEqual `onFail` failBadP "expected = in 'encoding' decl"
    f <- bracket (tok TokQuote) (commit $ tok TokQuote) freetext
    return (EncodingDecl f)

notationdecl :: XParser NotationDecl
notationdecl = do
    tok TokSpecialOpen
    tok (TokSpecial NOTATIONx)
    n <- name
    e <- either externalid publicid
    tok TokAnyClose `onFail` failBadP ("expected > terminating NOTATION decl "++n)
    return (NOTATION n e)

publicid :: XParser PublicID
publicid = do
    word "PUBLIC"
    p <- pubidliteral
    return (PUBLICID p)

entityvalue :: XParser EntityValue
entityvalue = do
 -- evs <- bracket (tok TokQuote) (commit $ tok TokQuote) (many (peRef ev))
    tok TokQuote
    pn <- posn
    evs <- many ev
    tok TokQuote `onFail` failBadP "expected quote to terminate entityvalue"
    -- quoted text must be rescanned for possible PERefs
    st <- stGet
 -- Prelude.either failBad (return . EntityValue) . fst3 $
    return . EntityValue . fst3 $
                (runParser (many ev) st
                         (reLexEntityValue (\s-> stringify (lookupPE s st))
                                           pn
                                           (flattenEV (EntityValue evs))))
  where
    stringify (Just (PEDefEntityValue ev)) = Just (flattenEV ev)
    stringify _ = Nothing

ev :: XParser EV
ev =
    oneOf' [ ("string",    (string`onFail`freetext) >>= return . EVString)
           , ("reference", reference >>= return . EVRef)
           ]
      `adjustErr` ("looking for entity value,\n"++)

attvalue :: XParser AttValue
attvalue = do
    avs <- bracket (tok TokQuote) (commit $ tok TokQuote)
                   (many (either freetext reference))
    return (AttValue avs)

systemliteral :: XParser SystemLiteral
systemliteral = do
    s <- bracket (tok TokQuote) (commit $ tok TokQuote) freetext
    return (SystemLiteral s)            -- note: refs &...; not permitted

pubidliteral :: XParser PubidLiteral
pubidliteral = do
    s <- bracket (tok TokQuote) (commit $ tok TokQuote) freetext
    return (PubidLiteral s)             -- note: freetext is too liberal here

-- | Return parsed freetext (i.e. until the next markup)
chardata :: XParser CharData
chardata = freetext

