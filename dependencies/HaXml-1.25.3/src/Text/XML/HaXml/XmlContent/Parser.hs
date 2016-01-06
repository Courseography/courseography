{-# LANGUAGE ExistentialQuantification #-}

-- | The class 'XmlContent' is a kind of replacement for Read and Show:
--   it provides conversions between a generic XML tree representation
--   and your own more specialised typeful Haskell data trees.
--
--   If you are starting with a set of Haskell datatypes, use DrIFT to
--   derive instances of this class for you:
--       http:\/\/repetae.net\/john\/computer\/haskell\/DrIFT
--   If you are starting with an XML DTD, use HaXml's tool DtdToHaskell
--   to generate both the Haskell types and the corresponding instances.
--
--   This unified class interface replaces two previous (somewhat similar)
--   classes: Haskell2Xml and Xml2Haskell.  There was no real reason to have
--   separate classes depending on how you originally defined your datatypes.
--   However, some instances for basic types like lists will depend on which 
--   direction you are using.  See Text.XML.HaXml.XmlContent and
--   Text.XML.HaXml.XmlContent.Haskell.

--   The methods 'toContents' and 'parseContents' convert a value to and from
--   a generic internal representation of an XML document /without/ a DTD.
--   The functions 'toXml' and 'fromXml' convert a value to and from a generic
--   internal representation of an XML document /including/ a DTD.
--   The functions 'readXml' and 'showXml' convert to and from Strings.
--   The functions 'fReadXml' and 'fWriteXml' do the conversion to and from
--   the given filenames.
--   The functions 'hGetXml' and 'hPutXml' do the conversion to and from
--   the given file handles.
--   (See the type signatures.)
--

module Text.XML.HaXml.XmlContent.Parser
  ( -- * Re-export the relevant set of generic XML document type definitions
    Document(..)
  , Element(..)
  , ElemTag(..)
  , Content(..)
  , Attribute()
  , AttValue(..)
  , Prolog(..)
  , Reference(..)
  -- * The enabling classes, that define parsing\/unparsing between Haskell
  --   datatypes and a generic XML representation.
  , XmlContent(..)
  , XmlAttributes(..)
  , XmlAttrType(..)
  -- ** Auxiliaries for writing parsers in the XmlContent class
  , module Text.ParserCombinators.Poly
  , XMLParser
  , content, posnElement, element, interior, inElement, text, attributes
  , posnElementWith, elementWith, inElementWith
  , choice, definite -- ???
  -- ** Auxiliaries for generating in the XmlContent class
  , mkElem, mkElemC, mkAttr
  , toText, toCData
  -- ** Auxiliaries for the attribute-related classes
  , maybeToAttr, defaultToAttr
  , definiteA, defaultA, possibleA, fromAttrToStr, toAttrFrStr
  , Defaultable(..)
  , str2attr, attr2str, attval
  , catMaybes   -- re-exported from Maybe
  -- * Explicit representation of Haskell datatype information
  --   (for conversion to a DTD)
  , module Text.XML.HaXml.TypeMapping
  -- * Types useful for some content models
  , List1(..)
  , ANYContent(..)
  ) where

--import System.IO
import Data.Maybe (catMaybes)
import Data.Char  (chr, isSpace)

import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces
import Text.XML.HaXml.TypeMapping
import Text.XML.HaXml.Posn     (Posn)
import Text.XML.HaXml.Verbatim (Verbatim(verbatim))

import Text.ParserCombinators.Poly

--  #define DEBUG

#if defined(DEBUG)
import Debug.Trace(trace)
debug :: a -> String -> a
v `debug` s = trace s v
#else
debug :: t -> t1 -> t
v `debug` _ = v
#endif


------------------------------------------------------------------------
-- | Read a single attribute called "value".
attval :: (Read a) => Element i -> a
attval (Elem _ [(_{-N "value"-},v@(AttValue _))] []) = read (show v)

-- | Generate a single attribute.
mkAttr :: String -> String -> Attribute
mkAttr n v = (N n, AttValue [Left v])

-- | Generate an element with no attributes, named for its HType.
mkElem :: XmlContent a => a -> [Content ()] -> Content ()
mkElem x cs  = CElem (Elem (N (showHType (toHType x) "")) [] cs) ()

-- | Generate an element with no attributes, named directly.
mkElemC :: String -> [Content ()] -> Content ()
mkElemC x cs = CElem (Elem (N x) [] cs) ()

-- | Turn a simple string into XML text.
toText :: String -> [Content ()]
toText s = [CString False s ()]

-- | Turn a string into an XML CDATA section.
--   (i.e. special characters like '&' are preserved without interpretation.)
toCData :: String -> [Content ()]
toCData s = [CString True s ()]


------------------------------------------------------------------------

-- | We need a parsing monad for reading generic XML Content into specific
--   datatypes.  This is a specialisation of the Text.ParserCombinators.Poly
--   ones, where the input token type is fixed as XML Content.
type XMLParser a = Parser (Content Posn) a


------------------------------------------------------------------------
-- Some useful parsing combinators
------------------------------------------------------------------------

-- | The most primitive combinator for XMLParser - get one content item.
content :: String -> XMLParser (Content Posn)
content word = next `adjustErr` (++" when expecting "++word)

-- | Get the next content element, checking that it has one of the required
--   tags, using the given matching function.
--   (Skips over comments and whitespace, rejects text and refs.
--    Also returns position of element.)
posnElementWith :: (String->String->Bool) -> [String]
                   -> XMLParser (Posn, Element Posn)
posnElementWith match tags = do
    { c <- content (formatted tags)
    ; case c of
          CElem e@(Elem t _ _) pos
              | any (match (localName t)) tags -> return (pos, e)
              | otherwise   -> fail ("Found a <"++printableName t
                                     ++">, but expected "
                                     ++formatted tags++"\nat "++show pos)
          CString b s pos
              | not b && all isSpace s -> posnElementWith match tags
                                                        -- ignore blank space
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

-- | A specialisation of @posnElementWith (==)@.
posnElement :: [String] -> XMLParser (Posn, Element Posn)
posnElement = posnElementWith (==)

-- | Get the next content element, checking that it has one of the required
--   tags.  (Skips over comments and whitespace, rejects text and refs.)
element :: [String] -> XMLParser (Element Posn)
element tags = fmap snd (posnElement tags)
                                `debug` ("Element: "++unwords tags++"\n")

-- | Like element, only permits a more flexible match against the tagname.
elementWith :: (String->String->Bool) -> [String] -> XMLParser (Element Posn)
elementWith match tags = fmap snd (posnElementWith match tags)
                                `debug` ("Element: "++unwords tags++"\n")

-- | Run an XMLParser on the contents of the given element (i.e. not on the
--   current monadic content sequence), checking that the contents are
--   exhausted, before returning the calculated value within the current
--   parser context.
interior :: Element Posn -> XMLParser a -> XMLParser a
interior (Elem e _ cs) p =
    case runParser p cs of
        (Left msg, _) -> fail msg
        (Right x, []) -> return x
        (Right x, ds@(d:_))
            | all onlyMisc ds -> return x
            | otherwise       -> fail ("Too many elements inside <"
                                      ++printableName e++"> at\n"
                                      ++show (info d)++"\n"
                                      ++"Found excess: "
                                      ++verbatim (take 7 ds)
                                      ++"\n[...]")
  where onlyMisc (CMisc _ _) = True
        onlyMisc (CString False s _) | all isSpace s = True
        onlyMisc _ = False

-- | A combination of element + interior.
inElement :: String -> XMLParser a -> XMLParser a
inElement tag p = do { e <- element [tag]; commit (interior e p) }

-- | A combination of elementWith + interior.
inElementWith :: (String->String->Bool) -> String -> XMLParser a -> XMLParser a
inElementWith match tag p = do { e <- elementWith match [tag]
                               ; commit (interior e p) }

-- | Do some parsing of the attributes of the given element
attributes :: XmlAttributes a => Element Posn -> XMLParser a
attributes (Elem _ as _) = return (fromAttrs as)

-- | 'text' is a counterpart to 'element', parsing text content if it
--   exists.  Adjacent text and references are coalesced.
text :: XMLParser String
text = text' []
  where text' acc =
          do { c <- content "plain text"
             ; case c of
                 CString _ s _        -> text' (s:acc)
                 CRef (RefChar s) _   -> text' (("&#"++show s++";") :acc)
                 CRef (RefEntity s) _ -> text' (('&':s++";"):acc)
                 CMisc _ _            -> text' acc
                 CElem _ _         -> do { reparse [c] -- put it back!
                                         ; if null acc then fail "empty string"
                                           else return (concat (reverse acc))
                                         }
             }
          `onFail` ( if null acc then fail "empty string"
                     else return (concat (reverse acc)) )


-- | 'choice f p' means if parseContents succeeds, apply f to the result,
--   otherwise use the continuation parser.
choice :: XmlContent a => (a -> b) -> XMLParser b -> XMLParser b
choice cons (P other) =
    P (\cs-> case runParser parseContents cs of
                 (Left _, _)  -> other cs
                 (Right x, cs') -> Success cs' (cons x) )

--choice cons other = fmap cons parseContents `onFail` other

-- | not sure this is needed now.   'definite p' previously ensured that
--   an element was definitely present.  Now I think the monad might take
--   care of that for us.
definite :: XmlContent a => XMLParser a -> String -> String -> XMLParser a
definite p inner tag = P (\cs-> case runParser p cs of
                                   (Left _, cs')   -> Failure cs' msg'
                                   (Right x, cs')  -> Success cs' x )
  where msg' = "content error: expected "++inner++" inside <"++tag
               ++"> element\n"

------------------------------------------------------------------------

-- | The @XmlContent@ class promises that an XML Content element can be
--   converted to and from a Haskell value.
class HTypeable a => XmlContent a where
    -- | Convert from XML to Haskell
    parseContents :: XMLParser a
    -- | Convert from Haskell to XML
    toContents    :: a -> [Content ()]

    -- | Dummy functions (for most types): used /only/ in the Char instance
    --   for coercing lists of Char into String.
    xToChar       :: a -> Char
    xFromChar     :: Char -> a
    xToChar        = error "HaXml.XmlContent.xToChar used in error"
    xFromChar      = error "HaXml.XmlContent.xFromChar used in error"

-- | The @XmlAttributes@ class promises that a list of XML tag attributes
--   can be converted to and from a Haskell value.
class XmlAttributes a where
    fromAttrs :: [Attribute] -> a
    toAttrs   :: a -> [Attribute]
-- | The @XmlAttrType@ class promises that an attribute taking an XML
--   enumerated type can be converted to and from a Haskell value.
class XmlAttrType a where
    fromAttrToTyp :: String -> Attribute -> Maybe a
    toAttrFrTyp   :: String -> a -> Maybe Attribute


------------------------------------------------------------------------
-- Instances for some of the standard basic datatypes.
-- Both DtdToHaskell and Haskell2Xml share these instances.
------------------------------------------------------------------------

instance (XmlContent a, XmlContent b) => XmlContent (a,b) where
    toContents (a,b) = toContents a ++ toContents b
    parseContents    = do
        { a <- parseContents
        ; b <- parseContents
        ; return (a,b)
        }

instance (XmlContent a, XmlContent b, XmlContent c) => XmlContent (a,b,c) where
    toContents (a,b,c) = toContents a ++ toContents b ++ toContents c
    parseContents = do
        { a <- parseContents
        ; b <- parseContents
        ; c <- parseContents
        ; return (a,b,c)
        }

instance (XmlContent a, XmlContent b, XmlContent c, XmlContent d) =>
         XmlContent (a,b,c,d) where
    toContents (a,b,c,d) = toContents a ++ toContents b ++ toContents c
                           ++ toContents d
    parseContents = do
        { a <- parseContents
        ; b <- parseContents
        ; c <- parseContents
        ; d <- parseContents
        ; return (a,b,c,d)
        }

instance ( XmlContent a, XmlContent b, XmlContent c, XmlContent d
         , XmlContent e ) =>
         XmlContent (a,b,c,d,e) where
    toContents (a,b,c,d,e) = toContents a ++ toContents b ++ toContents c
                             ++ toContents d ++ toContents e
    parseContents = do
        { a <- parseContents
        ; b <- parseContents
        ; c <- parseContents
        ; d <- parseContents
        ; e <- parseContents
        ; return (a,b,c,d,e)
        }

instance ( XmlContent a, XmlContent b, XmlContent c, XmlContent d
         , XmlContent e, XmlContent f ) =>
         XmlContent (a,b,c,d,e,f) where
    toContents (a,b,c,d,e,f) = toContents a ++ toContents b ++ toContents c
                               ++ toContents d ++ toContents e ++ toContents f
    parseContents = do
        { a <- parseContents
        ; b <- parseContents
        ; c <- parseContents
        ; d <- parseContents
        ; e <- parseContents
        ; f <- parseContents
        ; return (a,b,c,d,e,f)
        }

instance ( XmlContent a, XmlContent b, XmlContent c, XmlContent d
         , XmlContent e, XmlContent f, XmlContent g ) =>
         XmlContent (a,b,c,d,e,f,g) where
    toContents (a,b,c,d,e,f,g)
        = toContents a ++ toContents b ++ toContents c ++ toContents d
          ++ toContents e ++ toContents f ++ toContents g
    parseContents = do
        { a <- parseContents
        ; b <- parseContents
        ; c <- parseContents
        ; d <- parseContents
        ; e <- parseContents
        ; f <- parseContents
        ; g <- parseContents
        ; return (a,b,c,d,e,f,g)
        }

instance ( XmlContent a, XmlContent b, XmlContent c, XmlContent d
         , XmlContent e, XmlContent f, XmlContent g, XmlContent h ) =>
         XmlContent (a,b,c,d,e,f,g,h) where
    toContents (a,b,c,d,e,f,g,h)
        = toContents a ++ toContents b ++ toContents c ++ toContents d
          ++ toContents e ++ toContents f ++ toContents g ++ toContents h
    parseContents = do
        { a <- parseContents
        ; b <- parseContents
        ; c <- parseContents
        ; d <- parseContents
        ; e <- parseContents
        ; f <- parseContents
        ; g <- parseContents
        ; h <- parseContents
        ; return (a,b,c,d,e,f,g,h)
        }

instance ( XmlContent a, XmlContent b, XmlContent c, XmlContent d
         , XmlContent e, XmlContent f, XmlContent g, XmlContent h
         , XmlContent i ) =>
         XmlContent (a,b,c,d,e,f,g,h,i) where
    toContents (a,b,c,d,e,f,g,h,i)
        = toContents a ++ toContents b ++ toContents c ++ toContents d
          ++ toContents e ++ toContents f ++ toContents g ++ toContents h
          ++ toContents i
    parseContents = do
        { a <- parseContents
        ; b <- parseContents
        ; c <- parseContents
        ; d <- parseContents
        ; e <- parseContents
        ; f <- parseContents
        ; g <- parseContents
        ; h <- parseContents
        ; i <- parseContents
        ; return (a,b,c,d,e,f,g,h,i)
        }

instance ( XmlContent a, XmlContent b, XmlContent c, XmlContent d
         , XmlContent e, XmlContent f, XmlContent g, XmlContent h
         , XmlContent i, XmlContent j ) =>
         XmlContent (a,b,c,d,e,f,g,h,i,j) where
    toContents (a,b,c,d,e,f,g,h,i,j)
        = toContents a ++ toContents b ++ toContents c ++ toContents d
          ++ toContents e ++ toContents f ++ toContents g ++ toContents h
          ++ toContents i ++ toContents j
    parseContents = do
        { a <- parseContents
        ; b <- parseContents
        ; c <- parseContents
        ; d <- parseContents
        ; e <- parseContents
        ; f <- parseContents
        ; g <- parseContents
        ; h <- parseContents
        ; i <- parseContents
        ; j <- parseContents
        ; return (a,b,c,d,e,f,g,h,i,j)
        }

instance ( XmlContent a, XmlContent b, XmlContent c, XmlContent d
         , XmlContent e, XmlContent f, XmlContent g, XmlContent h
         , XmlContent i, XmlContent j, XmlContent k ) =>
         XmlContent (a,b,c,d,e,f,g,h,i,j,k) where
    toContents (a,b,c,d,e,f,g,h,i,j,k)
        = toContents a ++ toContents b ++ toContents c ++ toContents d
          ++ toContents e ++ toContents f ++ toContents g ++ toContents h
          ++ toContents i ++ toContents j ++ toContents k
    parseContents = do
        { a <- parseContents
        ; b <- parseContents
        ; c <- parseContents
        ; d <- parseContents
        ; e <- parseContents
        ; f <- parseContents
        ; g <- parseContents
        ; h <- parseContents
        ; i <- parseContents
        ; j <- parseContents
        ; k <- parseContents
        ; return (a,b,c,d,e,f,g,h,i,j,k)
        }

instance ( XmlContent a, XmlContent b, XmlContent c, XmlContent d
         , XmlContent e, XmlContent f, XmlContent g, XmlContent h
         , XmlContent i, XmlContent j, XmlContent k, XmlContent l ) =>
         XmlContent (a,b,c,d,e,f,g,h,i,j,k,l) where
    toContents (a,b,c,d,e,f,g,h,i,j,k,l)
        = toContents a ++ toContents b ++ toContents c ++ toContents d
          ++ toContents e ++ toContents f ++ toContents g ++ toContents h
          ++ toContents i ++ toContents j ++ toContents k ++ toContents l
    parseContents = do
        { a <- parseContents
        ; b <- parseContents
        ; c <- parseContents
        ; d <- parseContents
        ; e <- parseContents
        ; f <- parseContents
        ; g <- parseContents
        ; h <- parseContents
        ; i <- parseContents
        ; j <- parseContents
        ; k <- parseContents
        ; l <- parseContents
        ; return (a,b,c,d,e,f,g,h,i,j,k,l)
        }

instance ( XmlContent a, XmlContent b, XmlContent c, XmlContent d
         , XmlContent e, XmlContent f, XmlContent g, XmlContent h
         , XmlContent i, XmlContent j, XmlContent k, XmlContent l
         , XmlContent m ) =>
         XmlContent (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    toContents (a,b,c,d,e,f,g,h,i,j,k,l,m)
        = toContents a ++ toContents b ++ toContents c ++ toContents d
          ++ toContents e ++ toContents f ++ toContents g ++ toContents h
          ++ toContents i ++ toContents j ++ toContents k ++ toContents l
          ++ toContents m
    parseContents = do
        { a <- parseContents
        ; b <- parseContents
        ; c <- parseContents
        ; d <- parseContents
        ; e <- parseContents
        ; f <- parseContents
        ; g <- parseContents
        ; h <- parseContents
        ; i <- parseContents
        ; j <- parseContents
        ; k <- parseContents
        ; l <- parseContents
        ; m <- parseContents
        ; return (a,b,c,d,e,f,g,h,i,j,k,l,m)
        }

instance ( XmlContent a, XmlContent b, XmlContent c, XmlContent d
         , XmlContent e, XmlContent f, XmlContent g, XmlContent h
         , XmlContent i, XmlContent j, XmlContent k, XmlContent l
         , XmlContent m, XmlContent n ) =>
         XmlContent (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    toContents (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
        = toContents a ++ toContents b ++ toContents c ++ toContents d
          ++ toContents e ++ toContents f ++ toContents g ++ toContents h
          ++ toContents i ++ toContents j ++ toContents k ++ toContents l
          ++ toContents m ++ toContents n
    parseContents = do
        { a <- parseContents
        ; b <- parseContents
        ; c <- parseContents
        ; d <- parseContents
        ; e <- parseContents
        ; f <- parseContents
        ; g <- parseContents
        ; h <- parseContents
        ; i <- parseContents
        ; j <- parseContents
        ; k <- parseContents
        ; l <- parseContents
        ; m <- parseContents
        ; n <- parseContents
        ; return (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
        }

instance ( XmlContent a, XmlContent b, XmlContent c, XmlContent d
         , XmlContent e, XmlContent f, XmlContent g, XmlContent h
         , XmlContent i, XmlContent j, XmlContent k, XmlContent l
         , XmlContent m, XmlContent n, XmlContent o ) =>
         XmlContent (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    toContents (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
        = toContents a ++ toContents b ++ toContents c ++ toContents d
          ++ toContents e ++ toContents f ++ toContents g ++ toContents h
          ++ toContents i ++ toContents j ++ toContents k ++ toContents l
          ++ toContents m ++ toContents n ++ toContents o
    parseContents = do
        { a <- parseContents
        ; b <- parseContents
        ; c <- parseContents
        ; d <- parseContents
        ; e <- parseContents
        ; f <- parseContents
        ; g <- parseContents
        ; h <- parseContents
        ; i <- parseContents
        ; j <- parseContents
        ; k <- parseContents
        ; l <- parseContents
        ; m <- parseContents
        ; n <- parseContents
        ; o <- parseContents
        ; return (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
        }


------------------------------------------------------------------------
-- Useful auxiliaries for "fromAttributes"
------------------------------------------------------------------------

-- | If an attribute is defaultable, then it either takes the default
--   value (which is omitted from the output), or a non-default value
--   (which obviously must be printed).
data Defaultable a  = Default a    | NonDefault a    deriving (Eq,Show)

searchMaybe :: (a -> Maybe b) -> [a] -> Maybe b
searchMaybe _ [] = Nothing
searchMaybe f (x:xs) =
    let fx = f x in
    case fx of
      Nothing  -> searchMaybe f xs
      (Just _) -> fx

maybeToAttr :: (String->a->Maybe Attribute) -> String -> Maybe a
               -> Maybe Attribute
maybeToAttr _ _ Nothing  = Nothing
maybeToAttr to n (Just v) = to n v

defaultToAttr :: (String->a->Maybe Attribute) -> String -> Defaultable a
                 -> Maybe Attribute
defaultToAttr _ _ (Default _)  = Nothing
defaultToAttr to n (NonDefault v) = to n v

definiteA :: (String->Attribute->Maybe a) -> String -> String
             -> [Attribute] -> a
definiteA from tag at as =
    case searchMaybe (from at) as of
      Nothing  -> error ("missing attribute "++at++" in tag <"++tag++">")
      (Just a) -> a

defaultA :: (String->Attribute->Maybe a) -> a -> String
            -> [Attribute] -> Defaultable a
defaultA from def at as =
    case searchMaybe (from at) as of
      Nothing  -> Default def
      (Just a) -> NonDefault a

possibleA :: (String->Attribute->Maybe a) -> String -> [Attribute] -> Maybe a
possibleA from at as = searchMaybe (from at) as

fromAttrToStr :: String -> Attribute -> Maybe String
fromAttrToStr n (n0,v)
        | n == localName n0   = Just (attr2str v)
        | otherwise           = Nothing

toAttrFrStr   :: String -> String -> Maybe Attribute
toAttrFrStr n v = Just (N n, str2attr v)

str2attr :: String -> AttValue
str2attr s =
    let f t =
          let (l,r) = span (\c-> not (elem c "\"&<>'")) t
          in if null r then [Left l]
             else Left l: Right (g (head r)): f (tail r)
        g '"'  = RefEntity "quot"
        g '&'  = RefEntity "amp"
        g '<'  = RefEntity "lt"
        g '>'  = RefEntity "gt"
        g '\'' = RefEntity "apos"
    in AttValue (f s)

attr2str :: AttValue -> String          -- really needs symbol table
attr2str (AttValue xs) =
    let f (Left s) = s
        f (Right (RefChar i))        = [chr i]
        f (Right (RefEntity "quot")) = "\""
        f (Right (RefEntity "amp"))  = "&"
        f (Right (RefEntity "lt"))   = "<"
        f (Right (RefEntity "gt"))   = ">"
        f (Right (RefEntity "apos")) = "'"
        f (Right _)                  = "*"  -- Ooops, ST needed here.
    in concatMap f xs

------------------------------------------------------------------------
--  New content-model types
------------------------------------------------------------------------

{-
data OneOf2 a b
data OneOf3 a b c
data OneOf4 a b c d
    ... etc are now defined (with instances) in module OneOfN.
-}

-- | A type corresponding to XML's ANY contentspec.
--   It is either a list of unconverted xml 'Content'
--   or some 'XmlContent'-able value.
--
-- Parsing functions (e.g. 'parseContents') will always produce 'UnConverted'.
-- Note: The Show instance for 'UnConverted' uses 'verbatim'.
data ANYContent = forall a . (XmlContent a, Show a) => ANYContent a
                | UnConverted [Content Posn]

instance Show ANYContent where
    show (UnConverted c) = "UnConverted " ++ (show $ map verbatim c)
    show (ANYContent a)  = "ANYContent " ++ (show a)

instance Eq ANYContent where
    a == b = show a == show b

-- | The List1 type represents lists with at least one element.
--   It is required for DTD content models that use + as a modifier.
data List1 a = NonEmpty [a]  deriving (Eq, Show)


------------------------------------------------------------------------
--  Instances for new content-model types
------------------------------------------------------------------------
instance (HTypeable a) => HTypeable (List1 a) where
    toHType m  = Defined "List1" [hx]
                         [Constr "NonEmpty" [hx] [List hx] {-Nothing-}]
               where (NonEmpty x) = m
                     hx = toHType x
instance (XmlContent a) => XmlContent (List1 a) where
    toContents (NonEmpty xs) = concatMap toContents xs
    parseContents = fmap NonEmpty $ many1 parseContents

instance HTypeable ANYContent where
    toHType _      = Prim "ANYContent" "ANY"
instance XmlContent ANYContent where
    toContents (ANYContent a)  = toContents a
    toContents (UnConverted s) = map (fmap (const ())) s
    parseContents = P (\cs -> Success [] (UnConverted cs))

------------------------------------------------------------------------
--
------------------------------------------------------------------------
