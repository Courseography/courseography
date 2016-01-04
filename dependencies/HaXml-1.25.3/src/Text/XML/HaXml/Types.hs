{- |
   This module defines an internal (generic) representation for XML
   documents including their DTDs.

   History:
   The original module was derived by hand from the XML specification,
   following the grammar precisely.  Then we simplified the types,
   removing layers of indirection and redundancy, and generally making
   things easier to work with.  Then we allowed PEReferences to be
   ubiquitous, by removing them from the types and resolving all
   PE references at parse-time.  Finally, we added a per-document
   symbol table for GEReferences, and a whitespace-significance flag
   for plaintext.
-}

module Text.XML.HaXml.Types
  (
  -- * A simple symbol table mapping strings (references) to values.
    SymTab
  -- ** Symbol table operations
  , emptyST
  , addST
  , lookupST

  -- * XML Types
  -- ** The top-level document container
  , Document(..)

  -- ** The main document content
  , Element(..)
  , ElemTag(..)
  , Content(..)
  , Attribute
  , AttValue(..)
  , info

  -- ** Administrative parts of the document
  , Prolog(..)
  , XMLDecl(..)
  , Misc(..)
  , ProcessingInstruction
  , SDDecl
  , VersionInfo
  , Comment
  , PITarget

  -- ** The DTD
  -- *** content model
  , DocTypeDecl(..)
  , MarkupDecl(..)
  , ExtSubset(..)
  , ExtSubsetDecl(..)
  , ElementDecl(..)
  , ContentSpec(..)
  , CP(..)
  , Modifier(..)
  , Mixed(..)

  -- *** attribute model
  , AttListDecl(..)
  , AttDef(..)
  , AttType(..)
  , TokenizedType(..)
  , EnumeratedType(..)
  , NotationType
  , Enumeration
  , DefaultDecl(..)
  , FIXED(..)

  -- *** conditional sections
  , ConditionalSect(..)
  , IncludeSect
  , IgnoreSect
  , Ignore(..)
  , IgnoreSectContents(..)

  -- ** References
  , Reference(..)
  , EntityRef
  , CharRef
  , PEReference

  -- ** Entities
  , EntityDecl(..)
  , GEDecl(..)
  , PEDecl(..)
  , EntityDef(..)
  , PEDef(..)
  , ExternalID(..)
  , NDataDecl(..)
  , TextDecl(..)
  , ExtParsedEnt(..)
  , ExtPE(..)
  , NotationDecl(..)
  , PublicID(..)
  , EncodingDecl(..)
  , EntityValue(..)
  , EV(..)
  , PubidLiteral(..)
  , SystemLiteral(..)

  -- ** Namespaces
  , QName(..)
  , Namespace(..)

  -- ** Basic value types
  , Name
  , Names
  , NmToken
  , NmTokens
  , CharData
  , CDSect
  ) where


{- A simple symbol table for storing macros whilst parsing. -}

type SymTab a = [(String,a)]

emptyST :: SymTab a
emptyST  = []

addST :: String -> a -> SymTab a -> SymTab a
addST n v = ((n,v):)

lookupST :: String -> SymTab a -> Maybe a
lookupST = lookup



{- XML types start here -}

-- | The symbol table stored in a document holds all its general entity
--   reference definitions.
data Document i = Document Prolog (SymTab EntityDef) (Element i) [Misc]
                  deriving (Eq, Show)
data Prolog     = Prolog (Maybe XMLDecl) [Misc] (Maybe DocTypeDecl) [Misc]
                  deriving (Eq, Show)
data XMLDecl    = XMLDecl VersionInfo (Maybe EncodingDecl) (Maybe SDDecl)
                  deriving (Eq, Show)
data Misc       = Comment Comment
                | PI ProcessingInstruction
                deriving (Eq, Show)

type ProcessingInstruction = (PITarget,String)

type SDDecl      = Bool
type VersionInfo = String
type Comment     = String
type PITarget    = String

data DocTypeDecl = DTD QName (Maybe ExternalID) [MarkupDecl]  deriving (Eq, Show)
data MarkupDecl  = Element  ElementDecl
                 | AttList  AttListDecl
                 | Entity   EntityDecl
                 | Notation NotationDecl
                 | MarkupMisc Misc
                 deriving (Eq, Show)

data ExtSubset     = ExtSubset (Maybe TextDecl) [ExtSubsetDecl]  deriving (Eq, Show)
data ExtSubsetDecl = ExtMarkupDecl MarkupDecl
                   | ExtConditionalSect ConditionalSect
                   deriving (Eq, Show)

data Element i = Elem QName [Attribute] [Content i] deriving (Eq, Show)
--  ElemTag is an intermediate type for parsing only
data ElemTag   = ElemTag QName [Attribute]
type Attribute = (QName, AttValue)
data Content i = CElem (Element i) i
               | CString Bool CharData i
                        -- ^ bool is whether whitespace is significant
               | CRef Reference i
               | CMisc Misc i
               deriving Show

-- custom instance of Eq, ignoring the informational elements.
instance Eq (Content i) where
    (CElem e _)     == (CElem e' _)       =  e==e'
    (CString b c _) == (CString b' c' _)  =  b==b' && c==c'
    (CRef r _)      == (CRef r' _)        =  r==r'
    (CMisc m _)     == (CMisc m' _)       =  m==m'

info :: Content t -> t
info (CElem _ i) = i
info (CString _ _ i) = i
info (CRef _ i) = i
info (CMisc _ i) = i

instance Functor Document where
  fmap f (Document p st e ms) = Document p st (fmap f e) ms
instance Functor Element where
  fmap f (Elem t as cs) = Elem t as (map (fmap f) cs)
instance Functor Content where
  fmap f (CElem e i)     = CElem (fmap f e) (f i)
  fmap f (CString b s i) = CString b s (f i)
  fmap f (CRef r i)      = CRef r (f i)
  fmap f (CMisc m i)     = CMisc m (f i)

data ElementDecl = ElementDecl QName ContentSpec deriving (Eq, Show)
data ContentSpec = EMPTY
                 | ANY
                 | Mixed Mixed
                 | ContentSpec CP
                 deriving (Eq, Show)
data CP = TagName QName Modifier
        | Choice [CP] Modifier
        | Seq [CP] Modifier
        deriving (Eq, Show)
data Modifier = None  -- ^ Just One
              | Query -- ^ Zero Or One
              | Star  -- ^ Zero Or More
              | Plus  -- ^ One Or More
              deriving (Eq, Show)
data Mixed = PCDATA
           | PCDATAplus [QName]
           deriving (Eq, Show)
data AttListDecl = AttListDecl QName [AttDef] deriving (Eq, Show)
data AttDef      = AttDef QName AttType DefaultDecl deriving (Eq, Show)
data AttType     = StringType
                 | TokenizedType TokenizedType
                 | EnumeratedType EnumeratedType
                 deriving (Eq, Show)
data TokenizedType = ID
                   | IDREF
                   | IDREFS
                   | ENTITY
                   | ENTITIES
                   | NMTOKEN
                   | NMTOKENS
                   deriving (Eq, Show)
data EnumeratedType = NotationType NotationType
                    | Enumeration Enumeration
                    deriving (Eq, Show)
type NotationType   = [Name]    -- nonempty list
type Enumeration    = [NmToken] -- nonempty list
data DefaultDecl    = REQUIRED
                    | IMPLIED
                    | DefaultTo AttValue (Maybe FIXED)
                    deriving (Eq, Show)
data FIXED          = FIXED deriving (Eq, Show)

data ConditionalSect = IncludeSect IncludeSect
                     | IgnoreSect IgnoreSect
                     deriving (Eq, Show)
type IncludeSect = [ExtSubsetDecl]
type IgnoreSect  = [IgnoreSectContents]
data Ignore      = Ignore deriving (Eq, Show)
data IgnoreSectContents = IgnoreSectContents Ignore [(IgnoreSectContents,Ignore)]  deriving (Eq, Show)

data Reference    = RefEntity EntityRef
                  | RefChar CharRef
                  deriving (Eq,Show)
type EntityRef    = Name
type CharRef      = Int
type PEReference  = Name

data EntityDecl   = EntityGEDecl GEDecl
                  | EntityPEDecl PEDecl
                  deriving (Eq, Show)
data GEDecl       = GEDecl Name EntityDef deriving (Eq, Show)
data PEDecl       = PEDecl Name PEDef deriving (Eq, Show)
data EntityDef    = DefEntityValue EntityValue
                  | DefExternalID ExternalID (Maybe NDataDecl)
                  deriving (Eq, Show)
data PEDef        = PEDefEntityValue EntityValue
                  | PEDefExternalID ExternalID deriving (Eq,Show)
data ExternalID   = SYSTEM SystemLiteral
                  | PUBLIC PubidLiteral SystemLiteral deriving (Eq,Show)
newtype NDataDecl = NDATA Name  deriving (Eq, Show)

data TextDecl       = TextDecl (Maybe VersionInfo) EncodingDecl  deriving (Eq, Show)
data ExtParsedEnt i = ExtParsedEnt (Maybe TextDecl) (Content i) deriving (Eq, Show)
data ExtPE          = ExtPE (Maybe TextDecl) [ExtSubsetDecl] deriving (Eq, Show)

data NotationDecl    = NOTATION Name (Either ExternalID PublicID) deriving (Eq, Show)
newtype PublicID     = PUBLICID PubidLiteral deriving (Eq, Show)
newtype EncodingDecl = EncodingDecl String deriving (Eq, Show)

-- | A QName is a (possibly) qualified name, in the sense of XML namespaces.
data QName    = N  Name
              | QN Namespace Name deriving (Eq,Show)
-- | Namespaces are not defined in the XML spec itself, but at
--       http://www.w3.org/TR/xml-names
data Namespace = Namespace { nsPrefix  :: String
                           , nsURI     :: String
                           }
                 deriving (Show)
instance Eq Namespace where
    p == q  =  nsURI p == nsURI q     -- this is the W3C spec's definition!
instance Ord QName where
    compare (N n)    (N m)    = compare n m
    compare (QN p n) (N m)    = LT
    compare (N n)    (QN q m) = GT
    compare (QN p n) (QN q m) = case compare (nsPrefix p) (nsPrefix q) of
                                  EQ -> compare n m
                                  r  -> r

type Name     = String           -- non-empty string
type Names    = [Name]           -- non-empty list
type NmToken  = String           -- non-empty string
type NmTokens = [NmToken]        -- non-empty list

data AttValue    = AttValue [Either String Reference] deriving Eq
instance Show AttValue where
  show (AttValue v) = concatMap decode v
    where
      decode (Left  w)               = w
      decode (Right (RefEntity ent)) = "&"++ent++";"
      decode (Right (RefChar cref))  = "&"++show cref++";"

data EntityValue = EntityValue [EV] deriving (Eq,Show)
data EV = EVString String
 --  -- | EVPERef PEReference
        | EVRef Reference  deriving (Eq,Show)
newtype PubidLiteral  = PubidLiteral String deriving (Eq,Show)
newtype SystemLiteral = SystemLiteral String deriving (Eq,Show)
type CharData         = String
type CDSect           = CharData

instance Eq ElemTag where
    (ElemTag n _) == (ElemTag m _)  = n==m
