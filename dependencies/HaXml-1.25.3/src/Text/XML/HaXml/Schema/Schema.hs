{-# LANGUAGE CPP, MultiParamTypeClasses, FunctionalDependencies,
             TypeSynonymInstances, ExistentialQuantification #-}
module Text.XML.HaXml.Schema.Schema
  ( SchemaType(..)
  , SimpleType(..) -- already exported by PrimitiveTypes
  , Extension(..)
  , Restricts(..)
  , FwdDecl(..)
  , getAttribute
  , between
  , Occurs(..)
  , parseSimpleType
  , parseText
  , AnyElement(..)
  , parseAnyElement
--  , module Text.XML.HaXml.XmlContent.Parser -- no, just the things below
  , Content(..)
  , XMLParser(..)
  , posnElement
  , posnElementWith
  , element
  , interior
  , text
  , module Text.ParserCombinators.Poly
  , module Text.Parse
--  , module Text.XML.HaXml.Schema.PrimitiveTypes
  , module Text.XML.HaXml.OneOfN
  , toXMLElement
  , toXMLText
  , toXMLAnyElement
  , toXMLAttribute
  , addXMLAttributes
  ) where

import Text.ParserCombinators.Poly
import Text.Parse

import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Namespaces (printableName)
import Text.XML.HaXml.XmlContent.Parser hiding (Document,Reference)
import Text.XML.HaXml.Schema.XSDTypeModel (Occurs(..))
import Text.XML.HaXml.Schema.PrimitiveTypes
import Text.XML.HaXml.Schema.PrimitiveTypes as Prim
import Text.XML.HaXml.OneOfN
import Text.XML.HaXml.Verbatim

-- | A SchemaType promises to interconvert between a generic XML
--   content tree and a Haskell value, according to the rules of XSD.
class SchemaType a where
    parseSchemaType :: String -> XMLParser a
    schemaTypeToXML :: String -> a -> [Content ()]

-- | A type t can extend another type s by the addition of extra elements
--   and/or attributes.  s is therefore the supertype of t.
class Extension t s {- - | t -> s -} where  -- fundep ill-advised.
    supertype :: t -> s

-- | A type t can restrict another type s, that is, t admits fewer values
--   than s, but all the values t does admit also belong to the type s.
class Restricts t s | t -> s where
    restricts :: t -> s

-- | A trick to enable forward-declaration of a type that will be defined
--   properly in another module, higher in the dependency graph. 'fd' is
--   a dummy type e.g. the empty @data FwdA@, where 'a' is the proper
--   @data A@, not yet available.
class FwdDecl fd a | fd -> a

-- | Given a TextParser for a SimpleType, make it into an XMLParser, i.e.
--   consuming textual XML content as input rather than a String.
parseSimpleType :: SimpleType t => XMLParser t
parseSimpleType = do s <- text
                     case runParser acceptingParser s of
                       (Left err, _) -> fail err
                       (Right v, "") -> return v
                       (Right v, _)  -> return v -- ignore trailing text

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | Between is a list parser that tries to ensure that any range
--   specification (min and max elements) is obeyed when parsing.
between :: PolyParse p => Occurs -> p a -> p [a]
between (Occurs Nothing  Nothing)  p = fmap (:[]) p
between (Occurs (Just i) Nothing)  p = return (++) `apply` exactly i p
                                                   `apply` many p
between (Occurs Nothing  (Just j)) p = upto j p
between (Occurs (Just i) (Just j)) p = return (++) `apply` exactly i p
                                                   `apply` upto (j-i) p

-- | Generated parsers will use 'getAttribute' as a convenient wrapper
--   to lift a SchemaAttribute parser into an XMLParser.
getAttribute :: (SimpleType a, Show a) =>
                String -> Element Posn -> Posn -> XMLParser a
getAttribute aname (Elem t as _) pos =
    case qnLookup aname as of
        Nothing  -> fail $ "attribute missing: " ++ aname
                           ++ " in element <" ++ printableName t
                           ++ "> at " ++ show pos
        Just atv -> case runParser acceptingParser (attr2str atv) of
                        (Right val, "")   -> return val
                        (Right val, rest) -> failBad $
                                               "Bad attribute value for "
                                               ++ aname ++ " in element <"
                                               ++ printableName t
                                               ++ ">:  got "++show val
                                               ++ "\n but trailing text: "
                                               ++ rest ++ "\n at " ++ show pos
                        (Left err,  rest) -> failBad $ err ++ " in attribute "
                                               ++ aname  ++ " of element <"
                                               ++ printableName t
                                               ++ "> at " ++ show pos
  where
    qnLookup :: String -> [(QName,a)] -> Maybe a
    qnLookup s = Prelude.lookup s . map (\(qn,v)-> (printableName qn, v))


-- | The <xsd:any> type.  Parsing will always produce an "UnconvertedANY".
data AnyElement = forall a . (SchemaType a, Show a) => ANYSchemaType a
                | UnconvertedANY (Content Posn)

instance Show AnyElement where
    show (UnconvertedANY c) = "Unconverted "++ show (verbatim c)
    show (ANYSchemaType a)  = "ANYSchemaType "++show a
instance Eq AnyElement where
    a == b  =  show a == show b
instance SchemaType AnyElement where
    parseSchemaType _ = parseAnyElement
    schemaTypeToXML _ = toXMLAnyElement

parseAnyElement :: XMLParser AnyElement
parseAnyElement = fmap UnconvertedANY next

-- | Parse the textual part of mixed content
parseText :: XMLParser String
parseText = text  -- from XmlContent.Parser
            `onFail` return ""

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

toXMLElement :: String -> [[Attribute]] -> [[Content ()]] -> [Content ()]
toXMLElement name attrs content =
    [CElem (Elem (N name) (concat attrs) (concat content)) ()]

toXMLText :: String -> [Content ()]
toXMLText text =
    [CString False text ()]

toXMLAnyElement :: AnyElement -> [Content ()]
toXMLAnyElement (UnconvertedANY c) = [fmap (const ()) c]
--toXMLAnyElement (ANYSchemaType x)  = [c]

toXMLAttribute :: (SimpleType a) => String -> a -> [Attribute]
toXMLAttribute name val = [ (N name, AttValue [Left (simpleTypeText val)]) ]

-- | For a ComplexType that is an extension of a SimpleType, it is necessary to
--   convert the value to XML first, then add in the extra attributes that
--   constitute the extension.
addXMLAttributes :: [[Attribute]] -> [Content ()] -> [Content ()]
addXMLAttributes extra [CElem (Elem n attrs content) ()] =
                       [CElem (Elem n (attrs++concat extra) content) ()]
addXMLAttributes _ x = x

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


{- examples
   --------

instance SchemaType FpMLSomething where
  parseSchemaType s = do (pos,e) <- posnElement [s]
                         commit $ do
                           a0 <- getAttribute "flirble" e pos
                           a1 <- getAttribute "binky" e pos
                           interior e $ do
                             c0 <- parseSchemaType "foobar"
                             c1 <- many $ parseSchemaType "quux"
                             c2 <- optional $ parseSchemaType "doodad"
                             c3 <- between (Occurs (Just 3) (Just 5))
                                            $ parseSchemaType "rinta"
                             c4 <- fmap OneOf2 (parseSchemaType "left")
                                   `onFail`
                                   fmap TwoOf2 (parseSchemaType "right")
                             return $ FpMLSomething a0 a1 c0 c1 c2 c3 c4
  schemaTypeToXML s x@FPMLSomething{} =
      toXMLElement s [ mkAttribute "flirble" (something_flirble x)
                     , mkAttribute "binky"   (something_binky x)
                     ]
          [             schemaTypeToXML "foobar"  (something_foobar x)
          , concatMap  (schemaTypeToXML "quux")   (something_quux x)
          , maybe []   (schemaTypeToXML "doodad") (something_doodad x)
          , concatMap  (schemaTypeToXML "rinta")  (something_rinta x)
          , foldOneOf2 (schemaTypeToXML "left")
                       (schemaTypeToXML "right")  (something_choice4 x)
          ]

instance SimpleType FpMLNumber where
    acceptingParser = ...
    simpleTypeText  = ...
-}


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Ensure that all primitive/simple types can also be used as elements.

#define SchemaInstance(TYPE)  instance SchemaType TYPE where { parseSchemaType s = do { e <- element [s]; interior e $ parseSimpleType; }; schemaTypeToXML s x = toXMLElement s [] [toXMLText (simpleTypeText x)] }

SchemaInstance(XsdString)
SchemaInstance(Prim.Boolean)
SchemaInstance(Prim.Base64Binary)
SchemaInstance(Prim.HexBinary)
SchemaInstance(Float)
SchemaInstance(Decimal)
SchemaInstance(Double)
SchemaInstance(Prim.AnyURI)
SchemaInstance(Prim.NOTATION)
SchemaInstance(Prim.Duration)
SchemaInstance(Prim.DateTime)
SchemaInstance(Prim.Time)
SchemaInstance(Prim.Date)
SchemaInstance(Prim.GYearMonth)
SchemaInstance(Prim.GYear)
SchemaInstance(Prim.GMonthDay)
SchemaInstance(Prim.GDay)
SchemaInstance(Prim.GMonth)
SchemaInstance(Prim.NormalizedString)
SchemaInstance(Prim.Token)
SchemaInstance(Prim.Language)
SchemaInstance(Prim.Name)
SchemaInstance(Prim.NCName)
SchemaInstance(Prim.ID)
SchemaInstance(Prim.IDREF)
SchemaInstance(Prim.IDREFS)
SchemaInstance(Prim.ENTITY)
SchemaInstance(Prim.ENTITIES)
SchemaInstance(Prim.NMTOKEN)
SchemaInstance(Prim.NMTOKENS)
SchemaInstance(Integer)
SchemaInstance(Prim.NonPositiveInteger)
SchemaInstance(Prim.NegativeInteger)
SchemaInstance(Prim.Long)
SchemaInstance(Int)
SchemaInstance(Prim.Short)
SchemaInstance(Prim.Byte)
SchemaInstance(Prim.NonNegativeInteger)
SchemaInstance(Prim.UnsignedLong)
SchemaInstance(Prim.UnsignedInt)
SchemaInstance(Prim.UnsignedShort)
SchemaInstance(Prim.UnsignedByte)
SchemaInstance(Prim.PositiveInteger)

