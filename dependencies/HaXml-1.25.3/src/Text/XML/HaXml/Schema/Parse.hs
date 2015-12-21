module Text.XML.HaXml.Schema.Parse
  ( module Text.XML.HaXml.Schema.Parse
  ) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Monoid (Monoid(mappend))
-- import Text.ParserCombinators.Poly
import Text.Parse    -- for String parsers

import Text.XML.HaXml.Types      (Name,QName(..),Namespace(..),Attribute(..)
                                 ,Content(..),Element(..),info)
import Text.XML.HaXml.Namespaces
import Text.XML.HaXml.Verbatim hiding (qname)
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Schema.XSDTypeModel as XSD
import Text.XML.HaXml.XmlContent.Parser (text)


-- | Lift boolean 'or' over predicates.
(|||) :: (a->Bool) -> (a->Bool) -> (a->Bool)
p ||| q = \v -> p v || q v

-- | Qualify an ordinary name with the XSD namespace.
xsd :: Name -> QName
xsd name = QN Namespace{nsPrefix="xsd",nsURI="http://www.w3.org/2001/XMLSchema"}
              name

-- | Predicate for comparing against an XSD-qualified name.  (Also accepts
--   unqualified names, but this is probably a bit too lax.  Doing it right
--   would require checking to see whether the current schema module's default
--   namespace is XSD or not.)
xsdTag :: String -> Content Posn -> Bool
xsdTag tag (CElem (Elem qn _ _) _)  =  qn == xsd tag || qn == (N tag)
xsdTag _   _                        =  False

-- | We need a Parser monad for reading from a sequence of generic XML
--   Contents into specific datatypes that model the structure of XSD
--   descriptions.  This is a specialisation of the polyparse combinators,
--   fixing the input token type.
type XsdParser a = Parser (Content Posn) a

-- | Get the next content element, checking that it matches some criterion
--   given by the predicate.
--   (Skips over comments and whitespace, rejects text and refs.
--    Also returns position of element.)
--   The list of strings argument is for error reporting - it usually
--   represents a list of expected tags.
posnElementWith :: (Content Posn->Bool) -> [String]
                   -> XsdParser (Posn,Element Posn)
posnElementWith match tags = do
    { c <- next `adjustErr` (++" when expecting "++formatted tags)
    ; case c of
        CElem e pos
            | match c   -> return (pos,e)
        CElem (Elem t _ _) pos
            | otherwise -> fail ("Found a <"++printableName t
                                 ++">, but expected "
                                 ++formatted tags++"\nat "++show pos)
        CString b s pos  -- ignore blank space
            | not b && all isSpace s -> posnElementWith match tags
            | otherwise -> fail ("Found text content, but expected "
                                 ++formatted tags++"\ntext is: "++s
                                 ++"\nat "++show pos)
        CRef r pos -> fail ("Found reference, but expected "
                            ++formatted tags++"\nreference is: "++verbatim r
                            ++"\nat "++show pos)
        CMisc _ _ -> posnElementWith match tags  -- skip comments, PIs, etc.
    }
  where
    formatted [t]  = "a <"++t++">"
    formatted tgs = "one of"++ concatMap (\t->" <"++t++">") tgs

-- | Get the next content element, checking that it has the required tag
--   belonging to the XSD namespace.
xsdElement :: Name -> XsdParser (Element Posn)
xsdElement n = fmap snd (posnElementWith (xsdTag n) ["xsd:"++n])

-- | Get the next content element, whatever it is.
anyElement :: XsdParser (Element Posn)
anyElement = fmap snd (posnElementWith (const True) ["any element"])

-- | Grab and parse any and all children of the next element.
allChildren :: XsdParser a -> XsdParser a
allChildren p = do e <- anyElement
                   interiorWith (const True) p e

-- | Run an XsdParser on the child contents of the given element (i.e. not
--   in the current monadic content sequence), filtering the children
--   before parsing, and checking that the contents are exhausted, before
--   returning the calculated value within the current parser context.
interiorWith :: (Content Posn->Bool) -> XsdParser a
                -> Element Posn -> XsdParser a
interiorWith keep (P p) (Elem e _ cs) = P $ \inp->
    tidy inp $
    case p (filter keep cs) of
        Committed r        -> r
        f@(Failure _ _)    -> f
        s@(Success [] _)   -> s
        Success ds@(d:_) a
            | all onlyMisc ds -> Success [] a
            | otherwise       -> Committed $
                                 Failure ds ("Too many elements inside <"
                                             ++printableName e++"> at\n"
                                             ++show (info d)++"\n\n"
                                             ++"Found excess: "
                                             ++verbatim (take 5 ds))
  where onlyMisc (CMisc _ _) = True
        onlyMisc (CString False s _) | all isSpace s = True
        onlyMisc _ = False

-- | Check for the presence (and value) of an attribute in the given element.
--   Absence results in failure.
attribute :: QName -> TextParser a -> Element Posn -> XsdParser a
attribute qn (P p) (Elem n as _) = P $ \inp->
    case lookup qn as of
        Nothing  -> Failure inp $ "attribute "++printableName qn
                                  ++" not present in <"++printableName n++">"
        Just atv -> tidy inp $
                    case p (show atv) of
                      Committed r   -> r
                      Failure z msg -> Failure z $
                                             "Attribute parsing failure: "
                                             ++printableName qn++"=\""
                                             ++show atv++"\": "++msg
                      Success [] v  -> Success [] v
                      Success xs _  -> Committed $ 
                                       Failure xs $
                                             "Attribute parsing excess text: "
                                             ++printableName qn++"=\""
                                             ++show atv++"\":\n  Excess is: "
                                             ++xs

-- | Grab any attributes that declare a locally-used prefix for a
--   specific namespace.
namespaceAttrs :: Element Posn -> XsdParser [Namespace]
namespaceAttrs (Elem _ as _) =
    return . map mkNamespace . filter (matchNamespace "xmlns") $ as
  where
    deQN (QN _ n) = n
    mkNamespace (attname,attval) = Namespace { nsPrefix = deQN attname
                                             , nsURI    = verbatim attval
                                             }

-- | Predicate for whether an attribute belongs to a given namespace.
matchNamespace :: String -> Attribute -> Bool
matchNamespace n (N m,     _) =   False  -- (n++":") `isPrefixOf` m
matchNamespace n (QN ns _, _) =   n == nsPrefix ns

-- | Tidy up the parsing context.
tidy :: t -> Result x a -> Result t a
tidy inp (Committed r) = tidy inp r
tidy inp (Failure _ m) = Failure inp m
tidy inp (Success _ v) = Success inp v

-- | Given a URI for a targetNamespace, and a list of Namespaces, tell
--   me the prefix corresponding to the targetNamespace.
targetPrefix :: Maybe TargetNamespace -> [Namespace] -> Maybe String
targetPrefix Nothing    _   = Nothing
targetPrefix (Just uri) nss = fmap nsPrefix $ lookupBy ((==uri).nsURI) nss

-- | An auxiliary you might expect to find in Data.List
lookupBy :: (a->Bool) -> [a] -> Maybe a
lookupBy p []     = Nothing
lookupBy p (y:ys) | p y       = Just y
                  | otherwise = lookupBy p ys

-- | Turn a qualified attribute value (two strings) into a qualified name
--   (QName), but excluding the case where the namespace prefix corresponds
--   to the targetNamespace of the current schema document.
qual :: Maybe TargetNamespace -> [Namespace] -> String-> String -> QName
qual tn nss pre nm = case targetPrefix tn nss of
                         Nothing             -> QN thisNS nm
                         Just p  | p/=pre    -> QN thisNS nm
                                 | otherwise -> N nm
    where thisNS = Namespace{ nsPrefix = pre
                            , nsURI = maybe "" nsURI $
                                      lookupBy ((==pre).nsPrefix) nss
                            }

-- Now for the real parsers.

-- | Parse a Schema declaration
schema = do
    e <- xsdElement "schema"
    commit $ do
        tn  <- optional (attribute (N "targetNamespace") uri e)
        nss <- namespaceAttrs e
        return Schema
          `apply` (attribute (N "elementFormDefault")    qform e
                   `onFail` return Unqualified)
          `apply` (attribute (N "attributeFormDefault")  qform e
                   `onFail` return Unqualified)
          `apply` optional (attribute (xsd "finalDefault") final e)
          `apply` optional (attribute (xsd "blockDefault") block e)
          `apply` return tn
          `apply` optional (attribute (N "version")       string e)
          `apply` return nss
          `apply` interiorWith (const True) (many (schemaItem (qual tn nss))) e

-- | Parse a (possibly missing) <xsd:annotation> element.
annotation :: XsdParser Annotation
annotation = do
    definiteAnnotation `onFail` return (NoAnnotation "missing")

-- | Parse a definitely-occurring <xsd:annotation> element.
definiteAnnotation :: XsdParser Annotation
definiteAnnotation = do
    e <- xsdElement "annotation"
    ( fmap Documentation $ interiorWith (xsdTag "documentation")
                                        (allChildren text)  e
      ) `onFail` (
      fmap AppInfo $ interiorWith (xsdTag "documentation")
                                        (allChildren text)  e
      ) `onFail` (
      return (NoAnnotation "failed to parse")
      )

-- | Parse a FormDefault attribute.
qform :: TextParser QForm
qform = do
    w <- word
    case w of
        "qualified"   -> return Qualified
        "unqualified" -> return Unqualified
        _             -> failBad "Expected \"qualified\" or \"unqualified\""

-- | Parse a Final or Block attribute.
final :: TextParser Final
final = do
    w <- word
    case w of
        "restriction" -> return NoRestriction
        "extension"   -> return NoExtension
        "#all"        -> return AllFinal
        _             -> failBad $ "Expected \"restriction\" or \"extension\""
                                   ++" or \"#all\""
block :: TextParser Block
block = final

-- | Parse a schema item (just under the toplevel <xsd:schema>)
schemaItem :: (String->String->QName) -> XsdParser SchemaItem
schemaItem qual = oneOf'
       [ ("xsd:include",        include)
       , ("xsd:import",         import_)
       , ("xsd:redefine",       (redefine qual))
       , ("xsd:annotation",     fmap Annotation     definiteAnnotation)
         --
       , ("xsd:simpleType",     fmap Simple           (simpleType qual))
       , ("xsd:complexType",    fmap Complex          (complexType qual))
       , ("xsd:element",        fmap SchemaElement    (elementDecl qual))
       , ("xsd:attribute",      fmap SchemaAttribute  (attributeDecl qual))
       , ("xsd:attributeGroup", fmap AttributeGroup   (attributeGroup qual))
       , ("xsd:group",          fmap SchemaGroup      (group_ qual))
   --  , ("xsd:notation",       notation)
-- sigh
       , ("xs:include",        include)
       , ("xs:import",         import_)
       , ("xs:redefine",       (redefine qual))
       , ("xs:annotation",     fmap Annotation     definiteAnnotation)
         --
       , ("xs:simpleType",     fmap Simple           (simpleType qual))
       , ("xs:complexType",    fmap Complex          (complexType qual))
       , ("xs:element",        fmap SchemaElement    (elementDecl qual))
       , ("xs:attribute",      fmap SchemaAttribute  (attributeDecl qual))
       , ("xs:attributeGroup", fmap AttributeGroup   (attributeGroup qual))
       , ("xs:group",          fmap SchemaGroup      (group_ qual))
   --  , ("xs:notation",       notation)
       ]

-- | Parse an <xsd:include>.
include :: XsdParser SchemaItem
include = do e <- xsdElement "include"
             commit $ return Include
                      `apply` attribute (N "schemaLocation") uri e
                      `apply` interiorWith (xsdTag "annotation") annotation e

-- | Parse an <xsd:import>.
import_ :: XsdParser SchemaItem
import_ = do e <- xsdElement "import"
             commit $ return Import
                      `apply` attribute (N "namespace")      uri e
                      `apply` attribute (N "schemaLocation") uri e
                      `apply` interiorWith (xsdTag "annotation") annotation e

-- | Parse a <xsd:redefine>.
redefine :: (String->String->QName) -> XsdParser SchemaItem
redefine q = do e <- xsdElement "redefine"
                commit $ return Redefine
                     `apply` attribute (N "schemaLocation") uri e
                     `apply` interiorWith (const True) (many (schemaItem q)) e

-- | Parse a <xsd:simpleType> decl.
simpleType :: (String->String->QName) -> XsdParser SimpleType
simpleType q = do
    e <- xsdElement "simpleType"
    n <- optional (attribute (N "name") name e)
    f <- optional (attribute (N "final") final e)
    a <- interiorWith (xsdTag "annotation") annotation e
    commit $ interiorWith (not . xsdTag "annotation") (simpleItem n f a) e
  where
    simpleItem n f a =
        do e  <- xsdElement "restriction"
           commit $ do
             a1 <- interiorWith (xsdTag "annotation") annotation e
             b  <- optional (attribute (N "base") (qname q) e)
             r  <- interiorWith (not . xsdTag "annotation")
                                (restrictType a1 b `onFail` restriction1 a1 b) e
             return (Restricted a n f r)
        `onFail`
        do e  <- xsdElement "list"
           commit $ do
             a1 <- interiorWith (xsdTag "annotation") annotation e
             t  <- attribute (N "itemType") (fmap Right (qname q)) e
                     `onFail`
                   interiorWith (xsdTag "simpleType")
                                (fmap Left (simpleType q)) e
                     `adjustErr`
                   (("Expected attribute 'itemType' or element <simpleType>\n"
                    ++"  inside <list> decl.\n")++)
             return (ListOf (a`mappend`a1) n f t)
        `onFail`
        do e  <- xsdElement "union"
           commit $ do
             a1 <- interiorWith (xsdTag "annotation") annotation e
             ts <- interiorWith (xsdTag "simpleType") (many (simpleType q)) e
             ms <- attribute (N "memberTypes") (many (qname q)) e
                   `onFail` return []
             return (UnionOf (a`mappend`a1) n f ts ms)
        `adjustErr`
        ("xsd:simpleType does not contain a restriction, list, or union\n"++)

    restriction1 a b = return (RestrictSim1 a b)
                            `apply` (return Restriction1 `apply` particle q)
    restrictType a b = return (RestrictType a b)
                            `apply` (optional (simpleType q))
                            `apply` many1 aFacet

aFacet :: XsdParser Facet
aFacet = foldr onFail (fail "Could not recognise simpleType Facet")
               (zipWith facet ["minInclusive","minExclusive","maxInclusive"
                              ,"maxExclusive","totalDigits","fractionDigits"
                              ,"length","minLength","maxLength"
                              ,"enumeration","whiteSpace","pattern"]
                              [OrderedBoundsMinIncl,OrderedBoundsMinExcl
                              ,OrderedBoundsMaxIncl,OrderedBoundsMaxExcl
                              ,OrderedNumericTotalDigits
                              ,OrderedNumericFractionDigits
                              ,UnorderedLength,UnorderedMinLength
                              ,UnorderedMaxLength,UnorderedEnumeration
                              ,UnorderedWhitespace,UnorderedPattern])

facet :: String -> FacetType -> XsdParser Facet
facet s t = do e <- xsdElement s
               v <- attribute (N "value") string e
               f <- attribute (N "fixed") bool e
                    `onFail` return False -- XXX check this
               a <- interiorWith (const True) annotation e
               return (Facet t a v f)

-- | Parse a <xsd:complexType> decl.
complexType :: (String->String->QName) -> XsdParser ComplexType
complexType q =
    do e  <- xsdElement "complexType"
       commit $ return ComplexType
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` optional (attribute (N "name") string e)
           `apply` (attribute (N "abstract") bool e `onFail` return False)
           `apply` optional (attribute (N "final") final e)
           `apply` optional (attribute (N "block") block e)
           `apply` (attribute (N "mixed") bool e `onFail` return False)
           `apply` interiorWith (not . xsdTag "annotation") (complexItem q) e

-- | Parse the alternative contents of a <xsd:complexType> decl.
complexItem :: (String->String->QName) -> XsdParser ComplexItem
complexItem q =
    ( do e <- xsdElement "simpleContent"
         commit $ return SimpleContent
                `apply` interiorWith (xsdTag "annotation") annotation e
                `apply` interiorWith (not.xsdTag "annotation") stuff e
    ) `onFail` (
      do e <- xsdElement "complexContent"
         commit $ return ComplexContent
                `apply` interiorWith (xsdTag "annotation") annotation e
                `apply` (attribute (N "mixed") bool e `onFail` return False)
                `apply` interiorWith (not.xsdTag "annotation") stuff e
    ) `onFail` (
      do fmap ThisType $ particleAttrs q
    )
  where
    stuff :: XsdParser (Either Restriction1 Extension)
    stuff =
      ( do e <- xsdElement "restriction"
           commit $ fmap Left $ return Restriction1 `apply` particle q
      ) `onFail` (
        do e <- xsdElement "extension"
           commit $ fmap Right $ return Extension
               `apply` interiorWith (xsdTag "annotation") annotation e
               `apply` attribute (N "base") (qname q) e
               `apply` interiorWith (not.xsdTag "annotation")
                                    (particleAttrs q) e
      )

-- | Parse a particle decl.
particle :: (String->String->QName) -> XsdParser Particle
particle q = optional (fmap Left (choiceOrSeq q) `onFail` fmap Right (group_ q))

-- | Parse a particle decl with optional attributes.
particleAttrs :: (String->String->QName) -> XsdParser ParticleAttrs
particleAttrs q = return PA `apply` particle q
                            `apply` many (fmap Left (attributeDecl q)
                                          `onFail`
                                          fmap Right (attributeGroup q))
                            `apply` optional anyAttr

-- | Parse an <xsd:all>, <xsd:choice>, or <xsd:sequence> decl.
choiceOrSeq :: (String->String->QName) -> XsdParser ChoiceOrSeq
choiceOrSeq q =
    do e <- xsdElement "all"
       commit $ return All
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` interiorWith (not.xsdTag "annotation")
                                (many (elementDecl q)) e
    `onFail`
    do e <- xsdElement "choice"
       commit $ return Choice
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` occurs e
           `apply` interiorWith (not.xsdTag "annotation")
                                (many (elementEtc q)) e
    `onFail`
    do e <- xsdElement "sequence"
       commit $ return Sequence
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` occurs e
           `apply` interiorWith (not.xsdTag "annotation")
                                (many (elementEtc q)) e

-- | Parse a <xsd:group> decl.
group_ :: (String->String->QName) -> XsdParser Group
group_ q = do e <- xsdElement "group"
              commit $ return Group
                `apply` interiorWith (xsdTag "annotation") annotation e
                `apply` (fmap Left (attribute (N "name") string e)
                         `onFail`
                         fmap Right (attribute (N "ref") (qname q) e))
                `apply` occurs e
                `apply` interiorWith (not.xsdTag "annotation")
                                     (optional (choiceOrSeq q)) e

-- | Parse an <xsd:element>, <xsd:group>, <xsd:all>, <xsd:choice>,
--   <xsd:sequence> or <xsd:any>.
elementEtc :: (String->String->QName) -> XsdParser ElementEtc
elementEtc q = fmap HasElement (elementDecl q)
             `onFail`
             fmap HasGroup (group_ q)
             `onFail`
             fmap HasCS (choiceOrSeq q)
             `onFail`
             fmap HasAny any_

-- | Parse an <xsd:any>.
any_ :: XsdParser Any
any_ = do e <- xsdElement "any"
          commit $ return Any
              `apply` interiorWith (xsdTag "annotation") annotation e
              `apply` (attribute (N "namespace") uri e
                       `onFail` return "##any")
              `apply` (attribute (N "processContents") processContents e
                       `onFail` return Strict)
              `apply` occurs e

-- | Parse an <xsd:anyAttribute>.
anyAttr :: XsdParser AnyAttr
anyAttr = do e <- xsdElement "anyAttribute"
             commit $ return AnyAttr
                 `apply` interiorWith (xsdTag "annotation") annotation e
                 `apply` (attribute (N "namespace") uri e
                          `onFail` return "##any")
                 `apply` (attribute (N "processContents") processContents e
                          `onFail` return Strict)

-- | Parse an <xsd:attributegroup>.
attributeGroup :: (String->String->QName) -> XsdParser AttrGroup
attributeGroup q =
    do e <- xsdElement "attributeGroup"
       commit $ return AttrGroup
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` (fmap Left (attribute (N "name") string e)
                    `onFail`
                    fmap Right (attribute (N "ref") (qname q) e))
           `apply` interiorWith (not.xsdTag "annotation") (many stuff) e
  where
    stuff = fmap Left (attributeDecl q) `onFail` fmap Right (attributeGroup q)

-- | Parse an <xsd:element> decl.
elementDecl :: (String->String->QName) -> XsdParser ElementDecl
elementDecl q =
    do e <- xsdElement "element"
       commit $ return ElementDecl
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` (fmap Left (nameAndType q e)
                    `onFail`
                    fmap Right (attribute (N "ref") (qname q) e))
           `apply` occurs e
           `apply` (attribute (N "nillable") bool e `onFail` return False)
           `apply` optional (attribute (N "substitutionGroup") (qname q) e)
           `apply` (attribute (N "abstract") bool e `onFail` return False)
           `apply` optional (attribute (xsd "final") final e)
           `apply` optional (attribute (xsd "block") block e)
           `apply` (attribute (xsd "form") qform e `onFail` return Unqualified)
           `apply` interiorWith (xsdTag "simpleType" ||| xsdTag "complexType")
                                (optional (fmap Left (simpleType q)
                                           `onFail`
                                           fmap Right (complexType q))) e
           `apply` interiorWith (xsdTag "unique" ||| xsdTag "key"
                                                 ||| xsdTag "keyRef")
                                (many (uniqueKeyOrKeyRef q)) e

-- | Parse name and type attributes.
nameAndType :: (String->String->QName) -> Element Posn -> XsdParser NameAndType
nameAndType q e = return NT `apply` attribute (N "name") string e
                            `apply` optional (attribute (N "type") (qname q) e)

-- | Parse an <xsd:attribute> decl.
attributeDecl :: (String->String->QName) -> XsdParser AttributeDecl
attributeDecl q =
    do e <- xsdElement "attribute"
       commit $ return AttributeDecl
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` (fmap Left (nameAndType q e)
                    `onFail`
                    fmap Right (attribute (N "ref") (qname q) e))
           `apply` (attribute (N "use") use e `onFail` return Optional)
           `apply` (optional (attribute (N "default") (fmap Left string) e
                              `onFail`
                              attribute (N "fixed") (fmap Right string) e))
           `apply` (attribute (xsd "form") qform e `onFail` return Unqualified)
           `apply` interiorWith (xsdTag "simpleType")
                                (optional (simpleType q)) e


-- | Parse an occurrence range from attributes of given element.
occurs :: Element Posn -> XsdParser Occurs
occurs e = return Occurs
               `apply` (optional $ attribute (N "minOccurs") parseDec e)
               `apply` (optional $ attribute (N "maxOccurs") maxDec e)
  where
    maxDec = parseDec
             `onFail`
             do isWord "unbounded"; return maxBound

-- | Parse a <xsd:unique>, <xsd:key>, or <xsd:keyref>.
uniqueKeyOrKeyRef :: (String->String->QName) -> XsdParser UniqueKeyOrKeyRef
uniqueKeyOrKeyRef q = fmap U unique `onFail`
                      fmap K key `onFail`
                      fmap KR (keyRef q)

-- | Parse a <xsd:unique>.
unique :: XsdParser Unique
unique =
    do e <- xsdElement "unique"
       commit $ return Unique
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` attribute (N "name") string e
           `apply` interiorWith (xsdTag "selector") selector e
           `apply` interiorWith (xsdTag "field") (many1 field_) e

-- | Parse a <xsd:key>.
key :: XsdParser Key
key =
    do e <- xsdElement "key"
       commit $ return Key
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` attribute (N "name") string e
           `apply` interiorWith (xsdTag "selector") selector e
           `apply` interiorWith (xsdTag "field") (many1 field_) e

-- | Parse a <xsd:keyref>.
keyRef :: (String->String->QName) -> XsdParser KeyRef
keyRef q =
    do e <- xsdElement "keyref"
       commit $ return KeyRef
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` attribute (N "name") string e
           `apply` attribute (N "refer") (qname q) e
           `apply` interiorWith (xsdTag "selector") selector e
           `apply` interiorWith (xsdTag "field") (many1 field_) e

-- | Parse a <xsd:selector>.
selector :: XsdParser Selector
selector =
    do e <- xsdElement "selector"
       commit $ return Selector
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` attribute (N "xpath") string e

-- | Parse a <xsd:field>.
field_ :: XsdParser Field
field_ =
    do e <- xsdElement "field"
       commit $ return Field
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` attribute (N "xpath") string e

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | Text parser for a URI (very simple, non-validating, probably incorrect).
uri :: TextParser String
uri = string

-- | Text parser for an arbitrary string consisting of possibly multiple tokens.
string :: TextParser String
string = fmap concat $ many (space `onFail` word)

space :: TextParser String
space = many1 $ satisfy isSpace

-- | Parse a textual boolean, i.e. "true", "false", "0", or "1"
bool :: TextParser Bool
bool = do w <- word
          case w of
            "true"  -> return True
            "false" -> return False
            "0"     -> return True
            "1"     -> return False
            _       -> fail "could not parse boolean value"

-- | Parse a "use" attribute value, i.e. "required", "optional", or "prohibited"
use :: TextParser Use
use = do w <- word
         case w of
           "required"   -> return Required
           "optional"   -> return Optional
           "prohibited" -> return Prohibited
           _            -> fail "could not parse \"use\" attribute value"

-- | Parse a "processContents" attribute, i.e. "skip", "lax", or "strict".
processContents :: TextParser ProcessContents
processContents =
    do w <- word
       case w of
         "skip"   -> return Skip
         "lax"    -> return Lax
         "strict" -> return Strict
         _        -> fail "could not parse \"processContents\" attribute value"

-- | Parse an attribute value that should be a QName.
qname :: (String->String->QName) -> TextParser QName
qname q = do a <- word
             ( do ":" <- word
                  b   <- many (satisfy (/=':'))
                  return (q a b)
               `onFail`
               do cs <- many next
                  return (N (a++cs)) )

-- | Parse an attribute value that should be a simple Name.
name :: TextParser Name
name = word
