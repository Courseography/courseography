module Text.XML.HaXml.Schema.XSDTypeModel
  ( module Text.XML.HaXml.Schema.XSDTypeModel
  ) where

import Data.Monoid hiding (Any)
import Text.XML.HaXml.Types      (Name,Namespace,QName)

data Schema        = Schema
                   --  { schema_annotation           :: Annotation
                       { schema_elementFormDefault   :: QForm
                       , schema_attributeFormDefault :: QForm
                       , schema_finalDefault         :: Maybe Final
                       , schema_blockDefault         :: Maybe Block
                       , schema_targetNamespace      :: Maybe TargetNamespace
                       , schema_version              :: Maybe String
                       , schema_namespaces           :: [Namespace]
                       , schema_items                :: [SchemaItem]
                       }
                     deriving (Eq,Show)
data SchemaItem    = Include    SchemaLocation Annotation
                   | Import URI SchemaLocation Annotation
                   | Redefine   SchemaLocation [SchemaItem]
                   | Annotation Annotation
                   --
                   | Simple          SimpleType
                   | Complex         ComplexType
                   | SchemaElement   ElementDecl
                   | SchemaAttribute AttributeDecl
                   | AttributeGroup  AttrGroup
                   | SchemaGroup     Group
               --  | Notation        Name
                     deriving (Eq,Show)

-- The "simple type" model

data SimpleType    = Primitive  { simple_primitive   :: PrimitiveType }
                   | Restricted { simple_annotation  :: Annotation
                                , simple_name        :: Maybe Name
                                , simple_final       :: Maybe Final
                                , simple_restriction :: Restriction
                                }
                   | ListOf     { simple_annotation  :: Annotation
                                , simple_name        :: Maybe Name
                                , simple_final       :: Maybe Final
                                  -- simpletype = element, qname = attribute
                                , simple_type        :: Either SimpleType QName
                                }
                   | UnionOf    { simple_annotation  :: Annotation
                                , simple_name        :: Maybe Name
                                , simple_final       :: Maybe Final
                                  -- union = elements
                                , simple_union       :: [SimpleType]
                                  -- members = attribute
                                , simple_members     :: [QName]
                                }
                     deriving (Eq,Show)

data Restriction   = RestrictSim1 { restrict_annotation :: Annotation
                                  , restrict_base       :: Maybe QName 
                                  , restrict_r1         :: Restriction1
                                  }
                   | RestrictType { restrict_annotation :: Annotation
                                  , restrict_base       :: Maybe QName
                                  , restrict_type       :: Maybe SimpleType
                                  , restrict_facets     :: [Facet]
                                  }
                     deriving (Eq,Show)

data Facet         = Facet { facet_facetType  :: FacetType
                           , facet_annotation :: Annotation
                           , facet_facetValue :: String
                           , facet_fixed      :: Bool
                           }
                     deriving (Eq,Show)

data FacetType     = OrderedBoundsMinIncl
                   | OrderedBoundsMinExcl
                   | OrderedBoundsMaxIncl
                   | OrderedBoundsMaxExcl
                   | OrderedNumericTotalDigits
                   | OrderedNumericFractionDigits
                   | UnorderedPattern
                   | UnorderedEnumeration
                   | UnorderedWhitespace
                   | UnorderedLength
                   | UnorderedMaxLength
                   | UnorderedMinLength
                     deriving (Eq,Show)

-- The "complex type" model

data ComplexType   = ComplexType
                       { complex_annotation :: Annotation
                       , complex_name       :: Maybe Name
                       , complex_abstract   :: Bool
                       , complex_final      :: Maybe Final
                       , complex_block      :: Maybe Block
                       , complex_mixed      :: Bool
                       , complex_content    :: ComplexItem
                       }
                     deriving (Eq,Show)
data ComplexItem   = SimpleContent
                       { ci_annotation :: Annotation
                       , ci_stuff      :: (Either Restriction1 Extension)
                       }
                   | ComplexContent
                       { ci_annotation :: Annotation
                       , ci_mixed      :: Bool
                       , ci_stuff      :: (Either Restriction1 Extension)
                       }
                   | ThisType
                       { ci_thistype   :: ParticleAttrs
                       }
                     deriving (Eq,Show)

data Restriction1  = Restriction1 Particle
                     deriving (Eq,Show)
data Extension     = Extension
                       { extension_annotation :: Annotation
                       , extension_base       :: QName
                       , extension_newstuff   :: ParticleAttrs
                       }
                     deriving (Eq,Show)

type Particle      = Maybe (Either ChoiceOrSeq Group)
data ParticleAttrs = PA Particle [Either AttributeDecl AttrGroup]
                        (Maybe AnyAttr)
                     deriving (Eq,Show)
data Group         = Group
                       { group_annotation :: Annotation
                       , group_nameOrRef  :: Either Name QName
                       , group_occurs     :: Occurs
                       , group_stuff      :: Maybe ChoiceOrSeq
                       }
                     deriving (Eq,Show)

data ChoiceOrSeq   = All      Annotation [ElementDecl]
                   | Choice   Annotation Occurs [ElementEtc]
                   | Sequence Annotation Occurs [ElementEtc]
                     deriving (Eq,Show)
data ElementEtc    = HasElement ElementDecl
                   | HasGroup   Group
                   | HasCS      ChoiceOrSeq
                   | HasAny     Any
                     deriving (Eq,Show)

data Any           = Any
                       { any_annotation      :: Annotation
                       , any_namespace       :: URI
                       , any_processContents :: ProcessContents
                       , any_occurs          :: Occurs
                       }
                     deriving (Eq,Show)
data AnyAttr       = AnyAttr
                       { anyattr_annotation      :: Annotation
                       , anyattr_namespace       :: URI
                       , anyattr_processContents :: ProcessContents
                       }
                     deriving (Eq,Show)

data AttrGroup     = AttrGroup
                       { attrgroup_annotation :: Annotation
                       , attrgroup_nameOrRef  :: Either Name QName
                       , attrgroup_stuff      :: [Either AttributeDecl
                                                         AttrGroup]
                       }
                     deriving (Eq,Show)

data ElementDecl   = ElementDecl
                       { elem_annotation :: Annotation
                       , elem_nameOrRef  :: Either NameAndType QName
                       , elem_occurs     :: Occurs
                       , elem_nillable   :: Nillable
                       , elem_substGroup :: Maybe QName
                       , elem_abstract   :: Bool
                       , elem_final      :: Maybe Final
                       , elem_block      :: Maybe Block
                       , elem_form       :: QForm
                       , elem_content    :: Maybe (Either SimpleType
                                                          ComplexType)
                       , elem_stuff      :: [ UniqueKeyOrKeyRef ]
                       }
                     deriving (Eq,Show)
data NameAndType   = NT { theName :: Name, theType :: Maybe QName }
                     deriving (Eq,Show)


data AttributeDecl = AttributeDecl
                       { attr_annotation :: Annotation
                       , attr_nameOrRef  :: Either NameAndType QName
                       , attr_use        :: Use
                       , attr_defFixed   :: Maybe (Either DefaultValue
                                                          FixedValue)
                       , attr_form       :: QForm
                       , attr_simpleType :: Maybe SimpleType
                       }
                     deriving (Eq,Show)


data UniqueKeyOrKeyRef
                   = U  Unique
                   | K  Key
                   | KR KeyRef
                     deriving (Eq,Show)

data Unique        = Unique
                       { unique_annotation :: Annotation
                       , unique_name       :: Name
                       , unique_selector   :: Selector
                       , unique_fields     :: [Field]
                       }
                     deriving (Eq,Show)
data Key           = Key
                       { key_annotation :: Annotation
                       , key_name       :: Name
                       , key_selector   :: Selector
                       , key_fields     :: [Field]
                       }
                     deriving (Eq,Show)
data KeyRef        = KeyRef
                       { keyref_annotation :: Annotation
                       , keyref_name       :: Name
                       , keyref_refer      :: QName
                       , keyref_selector   :: Selector
                       , keyref_fields     :: [Field]
                       }
                     deriving (Eq,Show)
data Selector        = Selector
                       { selector_annotation :: Annotation
                       , selector_xpath      :: String
                       }
                     deriving (Eq,Show)
data Field           = Field
                       { field_annotation :: Annotation
                       , field_xpath      :: String
                       }
                     deriving (Eq,Show)

data Occurs        = Occurs (Maybe Int) (Maybe Int)
                     deriving (Eq,Show)
data Use           = Required | Optional | Prohibited
                     -- (1,1) |   (0,1)  |   (0,0) -- corresp. to Occurs values
                     deriving (Eq,Show)

data PrimitiveType = String | Boolean | Decimal | Float | Double
                   | Duration | DateTime | Time | Date
                   | GYearMonth | GYear | GMonthDay | GDay | GMonth
                   | Base64Binary | HexBinary
                   | AnyURI | QName | Notation
                     deriving (Eq,Show)
               

data MyRestriction = Range Occurs
                   | Pattern Regexp
                   | Enumeration [String]
                     deriving (Eq,Show)
type Mixed         = Bool
type Nillable      = Bool
type Fixed         = Bool

data Annotation    = Documentation String
                   | AppInfo String
                   | NoAnnotation String
                     deriving (Eq,Show)

data QForm         = Qualified | Unqualified -- only matters for locally decl'd
                     deriving (Eq,Show)
type TargetNamespace
                   = URI
data Final         = NoExtension | NoRestriction | AllFinal
                     deriving (Eq,Show)
type Block         = Final
data ProcessContents
                   = Skip | Lax | Strict
                     deriving (Eq,Show)

{-
data Constraint    = Unique Selector [Field]
                   | Key    Selector [Field]
                   | KeyRef Selector [Field]
                     deriving (Eq,Show)
type Selector      = String	-- XPath query for scope of constraint
type Field         = String	-- XPath query for entity being constrained
-}

-- check all of the following.
type SchemaLocation= String
type DefaultValue  = String
type FixedValue    = String
type Regexp        = String
type URI           = String
type TypeName      = String

instance Monoid Annotation where
  mempty = NoAnnotation "Monoid.mempty <Annotation>"
  (Documentation d) `mappend` (Documentation e) = Documentation (d++"\n"++e)
  _                 `mappend` (Documentation e) = Documentation e
  ann               `mappend` _                 = ann          

-- This instance is pretty unsatisfactory, and is useful only for
-- building environments involving recursive modules.  The /mappend/
-- method is left-biased, and the /mempty/ value contains lots of
-- undefined values.
instance Monoid Schema where
  mempty        = Schema{ schema_items=[] }
  s `mappend` t = s{ schema_items = schema_items s ++ schema_items t }

