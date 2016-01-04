{-# LANGUAGE TypeSynonymInstances #-}
module Text.XML.HaXml.Schema.PrimitiveTypes
  ( -- * Type class for parsing simpleTypes
    SimpleType(..)
  , module Text.Parse
  , -- * Primitive XSD datatypes
    XsdString(..)
  , Boolean(..)
  , Base64Binary(..)
  , HexBinary(..)
  , Float(..)
  , Decimal(..)
  , Double(..)
  , AnyURI(..)
  , QName(..)
  , NOTATION(..)
  , Duration(..)
  , DateTime(..)
  , Time(..)
  , Date(..)
  , GYearMonth(..)
  , GYear(..)
  , GMonthDay(..)
  , GDay(..)
  , GMonth(..)
  , -- * Derived, yet builtin, datatypes
    NormalizedString(..)
  , Token(..)
  , Language(..)
  , Name(..)
  , NCName(..)
  , ID(..)
  , IDREF(..)
  , IDREFS(..)
  , ENTITY(..)
  , ENTITIES(..)
  , NMTOKEN(..)
  , NMTOKENS(..)
  , Integer(..)
  , NonPositiveInteger(..)
  , NegativeInteger(..)
  , Long(..)
  , Int(..)
  , Short(..)
  , Byte(..)
  , NonNegativeInteger(..)
  , UnsignedLong(..)
  , UnsignedInt(..)
  , UnsignedShort(..)
  , UnsignedByte(..)
  , PositiveInteger(..)
  ) where

import Text.Parse
import Data.Char as Char
--import Data.Time.LocalTime -- for dates and times?
import Text.XML.HaXml.Types (QName(..))
import Data.Int
import Data.Word

-- | Ultimately, an XML parser will find some plain text as the content
--   of a simpleType, which will need to be parsed.  We use a TextParser,
--   because values of simpleTypes can also be given elsewhere, e.g. as
--   attribute values in an XSD definition, e.g. to restrict the permissible
--   values of the simpleType.  Such restrictions are therefore implemented
--   as layered parsers.
class SimpleType a where
  acceptingParser :: TextParser a
  simpleTypeText  :: a -> String

-- * Primitive types

type Boolean      = Bool
newtype XsdString = XsdString    String    deriving (Eq,Show)
data Base64Binary = Base64Binary String    deriving (Eq,Show)
data HexBinary    = HexBinary    String    deriving (Eq,Show)
data AnyURI       = AnyURI       String    deriving (Eq,Show)
--data QName
data NOTATION     = NOTATION String -- or re-use NOTATION from HaXml.Types?
                                           deriving (Eq,Show)
data Decimal      = Decimal Double         deriving (Eq,Show)
--data Float
--data Double
data Duration     = Duration Bool Int Int Int Int Int Float  deriving (Eq,Show)

-- * All of the following temporal types are incompletely specified for now.
--   They should probably be mapped to something appropriate from the time
--   package?

data DateTime     = DateTime String        deriving (Eq,Show) -- LocalTime ?
data Time         = Time String            deriving (Eq,Show) -- TimeOfDay ?
data Date         = Date String            deriving (Eq,Show) -- Day ?
data GYearMonth   = GYearMonth String      deriving (Eq,Show) -- ??
data GYear        = GYear String           deriving (Eq,Show) -- ??
data GMonthDay    = GMonthDay String       deriving (Eq,Show) -- ??
data GDay         = GDay String            deriving (Eq,Show) -- ??
data GMonth       = GMonth String          deriving (Eq,Show) -- ??

isNext :: Char -> TextParser Char
isNext c = do d <- next
              if c==d then return c else fail ("expected "++c:", got "++d:".")

instance SimpleType Bool where
    acceptingParser = do w <- word
                         case w of "true"  -> return True;
                                   "false" -> return False
                                   "0"     -> return False;
                                   "1"     -> return True
                                   _       -> fail ("Not a bool: "++w)
    simpleTypeText False = "false"
    simpleTypeText True  = "true"
instance SimpleType XsdString where
    acceptingParser = fmap XsdString (many next)
    simpleTypeText (XsdString s) = s
instance SimpleType Base64Binary where
    acceptingParser = fmap Base64Binary (many (satisfy isAlphaNum `onFail`
                                               satisfy isSpace `onFail`
                                               satisfy (`elem`"+/=")))
    simpleTypeText (Base64Binary s) = s
instance SimpleType HexBinary where
    acceptingParser = fmap HexBinary (many (satisfy Char.isHexDigit))
    simpleTypeText (HexBinary s) = s
instance SimpleType AnyURI where
    acceptingParser = fmap AnyURI (many next) -- not very satisfactory
    simpleTypeText (AnyURI s) = s
instance SimpleType NOTATION where
    acceptingParser = fmap NOTATION (many next) -- not very satisfactory
    simpleTypeText (NOTATION s) = s

instance SimpleType Decimal where
    acceptingParser = fmap Decimal parse
    simpleTypeText (Decimal s) = show s	-- XXX FIXME: showGFloat?
instance SimpleType Float where
    acceptingParser  = parse
    simpleTypeText x = show x		-- XXX FIXME: showGFloat?
instance SimpleType Double where
    acceptingParser  = parse
    simpleTypeText x = show x		-- XXX FIXME: showGFloat?

instance SimpleType Duration where
    acceptingParser = return Duration `apply` (do isNext '-'; return False
                                               `onFail` return True)
                                      `discard` isNext 'P'
                                      `apply` ((parseDec `discard` isNext 'Y')
                                               `onFail` return 0)
                                      `apply` ((parseDec `discard` isNext 'M')
                                               `onFail` return 0)
                                      `apply` ((parseDec `discard` isNext 'D')
                                               `onFail` return 0)
                                      `discard` (isNext 'T'`onFail`return 'T')
                                      -- fix: T absent iff H:M:S absent also
                                      `apply` ((parseDec `discard` isNext 'H')
                                               `onFail` return 0)
                                      `apply` ((parseDec `discard` isNext 'M')
                                               `onFail` return 0)
                                      `apply` ((parseFloat `discard` isNext 'S')
                                               `onFail` return 0)
    simpleTypeText (Duration pos y m d h n s) =
        (if pos then "" else "-")++show y++"Y"++show m++"M"++show d++"D"
        ++"T"++show h++"H"++show n++"M"++show s++"S"

instance SimpleType DateTime where
    acceptingParser = fmap DateTime (many next)
 -- acceptingParser = fail "not implemented: simpletype parser for DateTime"
    simpleTypeText (DateTime x) = x
instance SimpleType Time where
    acceptingParser = fmap Time (many next)
 -- acceptingParser = fail "not implemented: simpletype parser for Time"
    simpleTypeText (Time x) = x
instance SimpleType Date where
    acceptingParser = fmap Date (many next)
 -- acceptingParser = fail "not implemented: simpletype parser for Date"
    simpleTypeText (Date x) = x
instance SimpleType GYearMonth where
    acceptingParser = fmap GYearMonth (many next)
 -- acceptingParser = fail "not implemented: simpletype parser for GYearMonth"
    simpleTypeText (GYearMonth x) = x
instance SimpleType GYear where
    acceptingParser = fmap GYear (many next)
 -- acceptingParser = fail "not implemented: simpletype parser for GYear"
    simpleTypeText (GYear x) = x
instance SimpleType GMonthDay where
    acceptingParser = fmap GMonthDay (many next)
 -- acceptingParser = fail "not implemented: simpletype parser for GMonthDay"
    simpleTypeText (GMonthDay x) = x
instance SimpleType GDay where
    acceptingParser = fmap GDay (many next)
 -- acceptingParser = fail "not implemented: simpletype parser for GDay"
    simpleTypeText (GDay x) = x
instance SimpleType GMonth where
    acceptingParser = fmap GMonth (many next)
 -- acceptingParser = fail "not implemented: simpletype parser for GMonth"
    simpleTypeText (GMonth x) = x

-- * Derived builtin types

newtype NormalizedString = Normalized String	deriving (Eq,Show)
newtype Token    = Token    String              deriving (Eq,Show)
newtype Language = Language String              deriving (Eq,Show)
newtype Name     = Name     String              deriving (Eq,Show)
newtype NCName   = NCName   String              deriving (Eq,Show)
newtype ID       = ID       String              deriving (Eq,Show)
newtype IDREF    = IDREF    String              deriving (Eq,Show)
newtype IDREFS   = IDREFS   String              deriving (Eq,Show)
newtype ENTITY   = ENTITY   String              deriving (Eq,Show)
newtype ENTITIES = ENTITIES String              deriving (Eq,Show)
newtype NMTOKEN  = NMTOKEN  String              deriving (Eq,Show)
newtype NMTOKENS = NMTOKENS String              deriving (Eq,Show)

instance SimpleType NormalizedString where
    acceptingParser = fmap Normalized (many next)
    simpleTypeText (Normalized x) = x
instance SimpleType Token where
    acceptingParser = fmap Token (many next)
    simpleTypeText (Token x) = x
instance SimpleType Language where
    acceptingParser = fmap Language (many next)
    simpleTypeText (Language x) = x
instance SimpleType Name where
    acceptingParser = fmap Name (many next)
    simpleTypeText (Name x) = x
instance SimpleType NCName where
    acceptingParser = fmap NCName (many next)
    simpleTypeText (NCName x) = x
instance SimpleType ID where
    acceptingParser = fmap ID (many next)
    simpleTypeText (ID x) = x
instance SimpleType IDREF where
    acceptingParser = fmap IDREF (many next)
    simpleTypeText (IDREF x) = x
instance SimpleType IDREFS where
    acceptingParser = fmap IDREFS (many next)
    simpleTypeText (IDREFS x) = x
instance SimpleType ENTITY where
    acceptingParser = fmap ENTITY (many next)
    simpleTypeText (ENTITY x) = x
instance SimpleType ENTITIES where
    acceptingParser = fmap ENTITIES (many next)
    simpleTypeText (ENTITIES x) = x
instance SimpleType NMTOKEN where
    acceptingParser = fmap NMTOKEN (many next)
    simpleTypeText (NMTOKEN x) = x
instance SimpleType NMTOKENS where
    acceptingParser = fmap NMTOKENS (many next)
    simpleTypeText (NMTOKENS x) = x

--data Integer
newtype NonPositiveInteger = NonPos   Integer   deriving (Eq,Show)
newtype NegativeInteger    = Negative Integer   deriving (Eq,Show)
newtype Long               = Long     Int64     deriving (Eq,Show)
--data Int
newtype Short              = Short    Int16     deriving (Eq,Show)
newtype Byte               = Byte     Int8      deriving (Eq,Show)
newtype NonNegativeInteger = NonNeg   Integer   deriving (Eq,Show)
newtype UnsignedLong       = ULong    Word64    deriving (Eq,Show)
newtype UnsignedInt        = UInt     Word32    deriving (Eq,Show)
newtype UnsignedShort      = UShort   Word16    deriving (Eq,Show)
newtype UnsignedByte       = UByte    Word8     deriving (Eq,Show)
newtype PositiveInteger    = Positive Integer   deriving (Eq,Show)

instance SimpleType Integer where
    acceptingParser = parse
    simpleTypeText  = show
instance SimpleType NonPositiveInteger where
    acceptingParser = fmap NonPos parse
    simpleTypeText (NonPos x) = show x
instance SimpleType NegativeInteger where
    acceptingParser = fmap Negative parse
    simpleTypeText (Negative x) = show x
instance SimpleType Long where
    acceptingParser = fmap (Long . fromInteger) parse
    simpleTypeText (Long x) = show x
instance SimpleType Int where
    acceptingParser = parse
    simpleTypeText  = show
instance SimpleType Short where
    acceptingParser = fmap (Short . fromInteger) parse
    simpleTypeText (Short x) = show x
instance SimpleType Byte where
    acceptingParser = fmap (Byte . fromInteger) parse
    simpleTypeText (Byte x) = show x
instance SimpleType NonNegativeInteger where
    acceptingParser = fmap NonNeg parse
    simpleTypeText (NonNeg x) = show x
instance SimpleType UnsignedLong where
    acceptingParser = fmap (ULong . fromInteger) parse
    simpleTypeText (ULong x) = show x
instance SimpleType UnsignedInt where
    acceptingParser = fmap (UInt . fromInteger) parse
    simpleTypeText (UInt x) = show x
instance SimpleType UnsignedShort where
    acceptingParser = fmap (UShort . fromInteger) parse
    simpleTypeText (UShort x) = show x
instance SimpleType UnsignedByte where
    acceptingParser = fmap (UByte . fromInteger) parse
    simpleTypeText (UByte x) = show x
instance SimpleType PositiveInteger where
    acceptingParser = fmap Positive parse
    simpleTypeText (Positive x) = show x

