-- | A type model for Haskell datatypes that bears a reasonable correspondence
--   to the XSD type model.
module Text.XML.HaXml.Schema.HaskellTypeModel
  ( module Text.XML.HaXml.Schema.HaskellTypeModel
  ) where

import Text.XML.HaXml.Schema.NameConversion
import Text.XML.HaXml.Schema.XSDTypeModel (Schema(..),Occurs)
import Text.XML.HaXml.Schema.Parse (lookupBy)
import Text.XML.HaXml.Types (QName(..),Namespace(..))
import Data.List (partition)

-- | Comments can be attached to most things, but not all of them will exist.
type Comment   = Maybe String

-- | The whole Haskell module.
data Module    = Module
                 { module_name        :: XName   -- the name of this module
                 , module_xsd_ns      :: Maybe XName -- xmlns:prefix for XSD
                 , module_re_exports  :: [Decl]  -- modules imported + exported
                 , module_import_only :: [Decl]  -- module + alias
                 , module_decls       :: [Decl]  -- the body of the module
                 }

-- | There are essentially simple types, and complex types, each of which
--   can be either restricted or extended.  There are four kinds of complex
--   type: choices, sequences, named groups, or a simple element with content.
data Decl
                 -- becomes type T = S
               = NamedSimpleType     XName XName Comment

                 -- becomes newtype T = T S
                 --       + instance Restricts T S where restricts ...
               | RestrictSimpleType  XName XName [Restrict] Comment

                 -- becomes data T  = T  S Tf
                 --       + data Tf = Tf {fields}
                 --       + instance Extension T S Tf where ...
               | ExtendSimpleType    XName XName [Attribute] Comment

                 -- becomes data T = Ta S0 | Tb S1 | Tc S2 | ...
               | UnionSimpleTypes    XName [XName] Comment

                 -- becomes data T = T_C0 | T_C1 | T_C2 | ...
               | EnumSimpleType      XName [(XName,Comment)] Comment

                 -- becomes data T  = T { singleattr, fields }
                 --   or    data T  = T { manyattr, singlefield }
                 --   or    data T  = T { t_attrs :: Ta, fields }
                 --       + data Ta = Ta { attributes }
               | ElementsAttrs XName [Element] [Attribute] Comment

                 -- or if T is abstract, it becomes
                 --         data T = T_A  A
                 --                | T_B  B
                 --                | FwdDecl fc c => T_C (fc->c) fc
                 --                | ...
                 --         data FwdC = FwdC -- because C is not yet in scope
                 --         instance FwdDecl FwdC C  -- later, at defn of C
                 --
                 -- In fact, it is better to move the declaration of type C
                 -- here, rather than use a FwdDecl proxy.  This will require
                 -- some patching later where C was originally declared.
                 --         data T = T_A  A
                 --                | T_B  B
                 --                | T_C  C -- but C not yet declared
                 --                | ...
                 --         data C = ... -- because C is not yet in scope
                 --         -- later, at true defn site of C, omit its decl.
                 --
                 -- An earlier solution was
                 --         class T a where parseT :: String -> XMLParser a
                 --         instance T A
                 --         instance T B
                 --         instance T C
                 -- but this is incorrect because the choice between A|B|C
                 -- rests with the input doc, not with the caller of the parser.
               | ElementsAttrsAbstract {-typename-}XName
                                       {-subtypes-}[(XName,Maybe XName)]
                                --  ^ [(type name, module where declared later)]
                                       Comment

                 -- becomes function
                 --    elementE :: Parser T
                 --    elementE = parseSchemaType "E"
               | ElementOfType Element
                 -- or, if E is abstract, with substitutionGroup {Foo,Bar},
                 --    elementE = fmap T_Foo elementFoo `onFail`
                 --               fmap T_Bar elementBar `onFail` ...
               | ElementAbstractOfType {-element name-}XName
                                       {-abstract type name-}XName
                                       {-substitute elems and fwddecls-}
                                           [(XName,Maybe XName)]
                                       Comment

                 -- becomes (global) data T = E0 e0 | E1 e1 | E2 e2 | E3 e3
                 -- becomes (local)  OneOfN e0 e1 e2 e3
               | Choice XName [Element] Comment

                 -- becomes data GroupT = GT e0 e1 e2 e3
               | Group  XName [Element] Comment

      {-         -- becomes data GroupT = GT e0 e1 e2 e3
               | GroupAttrs XName [Attribute] Comment
      -}
                 -- becomes newtype T = T S
                 --       + different (more restrictive) parser
               | RestrictComplexType  XName XName Comment

                 -- becomes data T  = T  {fields}
                 --       + instance Extension T S where ...
                 -- or when T extends an _abstract_ XSDtype S, defined in an
                 -- earlier module, it additionally has
                 --        instance FwdDecl FwdT T
               | ExtendComplexType XName XName [Element] [Attribute]
                                               [Element] [Attribute]
                                               {-FwdDecl req'd-}(Maybe XName)
                                               {-supertype abstract?-}Bool
                                               {-grandsupertypes-}[XName]
                                               Comment
                 -- or when T is itself abstract, extending an abstract type S
                 --        class T a where parseT :: String -> XMLParser a
                 --        instance (T a) => S a where parseS = parseT
               | ExtendComplexTypeAbstract XName XName
                                       {-subtypes-}[(XName,Maybe XName)]
                                       {-FwdDecl instnc req'd-}(Maybe XName)
                                       {-grandsupertypes-}[XName]
                                       Comment

                 -- becomes an import and re-export
               | XSDInclude XName Comment
                 -- becomes an import only
               | XSDImport  XName (Maybe XName) Comment
                 -- a top-level annotation
               | XSDComment Comment
                 deriving (Eq,Show)

data Element   = Element { elem_name     :: XName
                         , elem_type     :: XName
                         , elem_modifier :: Modifier
                         , elem_byRef    :: Bool
                         , elem_locals   :: [Decl]
                      -- , elem_abstract :: Bool
                         , elem_substs   :: Maybe [XName] -- substitutable elems
                         , elem_comment  :: Comment
                         }
               | OneOf   { elem_oneOf    :: [[Element]]
                         , elem_modifier :: Modifier
                         , elem_comment  :: Comment
                         }
               | AnyElem { elem_modifier :: Modifier
                         , elem_comment  :: Comment
                         }
               | Text -- for mixed content
                 deriving (Eq,Show)
data Attribute = Attribute { attr_name    :: XName
                           , attr_type    :: XName
                           , attr_required:: Bool
                           , attr_comment :: Comment
                           }
                 deriving (Eq,Show)

data Modifier  = Single
               | Optional
               | Range Occurs
                 deriving (Eq,Show)

-- | Restrictions on simpleType
data Restrict  = RangeR Occurs Comment
               | Pattern String{-really Regexp-} Comment
               | Enumeration [(String,Comment)]
               | StrLength Occurs Comment
                 deriving (Eq,Show)


-- | A helper for building the formal Module structure.
mkModule :: String -> Schema -> [Decl] -> Module
mkModule name schema decls =
                      Module { module_name        = XName $ N name
                             , module_xsd_ns      = xsdQualification
                                                      (schema_namespaces schema)
                             , module_re_exports  = reexports
                             , module_import_only = imports
                             , module_decls       = theRest
                             }
    where (reexports,other)   = partition xsdinclude decls
          (imports,  theRest) = partition xsdimport  other
          xsdinclude (XSDInclude _ _)  = True
          xsdinclude _                 = False
          xsdimport  (XSDImport _ _ _) = True
          xsdimport  _                 = False
          xsdQualification nss = fmap (XName . N . nsPrefix) $
                                      lookupBy ((==xsd).nsURI) nss
              where xsd = "http://www.w3.org/2001/XMLSchema"

