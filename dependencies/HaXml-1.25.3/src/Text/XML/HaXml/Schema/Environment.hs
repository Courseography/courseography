{-# LANGUAGE PatternGuards #-}
module Text.XML.HaXml.Schema.Environment
  ( module Text.XML.HaXml.Schema.Environment
  ) where

import Text.XML.HaXml.Types (QName(..),Name(..),Namespace(..))
import Text.XML.HaXml.Schema.XSDTypeModel
import Text.XML.HaXml.Schema.NameConversion (wordsBy)
import Text.XML.HaXml.Schema.Parse (targetPrefix)

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (foldl')

-- Some things we probably want to do.
-- * Build Maps from :
--       typename        to definition
--       element name    to definition
--       attribute name  to definition
--       (element) group to definition
--       attribute group to definition
--       abstract complextype to its extension types
--       substitution group to its substitutable elements
--       abstract/substGroup to defining module
-- * XSD types become top-level types in Haskell.
-- * XSD element decls also become top-level types in Haskell.
-- * Element groups get their own Haskell types too.
-- * Attributes and attribute groups do not become types, they are
--   simply constituent parts of an element.
-- * Resolve element/attribute references by inlining their names.

-- If a complextype definition includes nested in-line decls of other
-- types, we need to be able to lift them out to the top-level, then
-- refer to them by name only at the nested position(?)

-- When dealing with sub/supertype relationships, we often need to know all
-- of the subtypes of a supertype before some of the subtypes are actually
-- available in scope.  The environment must therefore first be closed
-- over all modules: the resulting type mapping (env_type) should be _copied_
-- across to (env_allTypes) in a fresh initial environment, which latter is
-- then used to rebuild the local scope from scratch.
-- Likewise, the mappings from supertype->subtype (env_extendty) and for
-- substitution groups (env_substGrp) also need to be global.

data Environment =  Environment
    { env_type      :: Map QName (Either SimpleType ComplexType)
                                 -- ^ type definitions in scope
    , env_allTypes  :: Map QName (Either SimpleType ComplexType)
                                 -- ^ all type definitions, regardless of scope
    , env_element   :: Map QName ElementDecl
    , env_attribute :: Map QName AttributeDecl
    , env_group     :: Map QName Group
    , env_attrgroup :: Map QName AttrGroup
    , env_namespace :: Map String{-URI-} String{-Prefix-}
    , env_extendty  :: Map QName [(QName,FilePath)] -- ^ supertype -> subtypes
    , env_substGrp  :: Map QName [(QName,FilePath)] -- ^ substitution groups
    , env_typeloc   :: Map QName FilePath           -- ^ where type is defined
    }

-- | An empty environment of XSD type mappings.
emptyEnv :: Environment
emptyEnv = Environment Map.empty Map.empty Map.empty Map.empty Map.empty
                       Map.empty Map.empty Map.empty Map.empty Map.empty

-- | Combine two environments (e.g. read from different interface files)
combineEnv :: Environment -> Environment -> Environment
combineEnv e1 e0 = Environment
    { env_type      = Map.union (env_type e1)      (env_type e0)
    , env_allTypes  = Map.union (env_allTypes e1)  (env_allTypes e0)
    , env_element   = Map.union (env_element e1)   (env_element e0)
    , env_attribute = Map.union (env_attribute e1) (env_attribute e0)
    , env_group     = Map.union (env_group e1)     (env_group e0)
    , env_attrgroup = Map.union (env_attrgroup e1) (env_attrgroup e0)
    , env_namespace = Map.union (env_namespace e1) (env_namespace e0)
    , env_extendty  = Map.unionWith (++) (env_extendty e1) (env_extendty e0)
    , env_substGrp  = Map.unionWith (++) (env_substGrp e1) (env_substGrp e0)
    , env_typeloc   = Map.union (env_typeloc e1)   (env_typeloc e0)
    }

-- | Build an environment of XSD type mappings from a schema module.
mkEnvironment :: FilePath -> Schema -> Environment -> Environment
mkEnvironment fp s init = foldl' item (addNS init (schema_namespaces s))
                                      (schema_items s)
  where
    -- think about qualification, w.r.t targetNamespace, elementFormDefault, etc
    item env (Include _ _)       = env
    item env (Import _ _ _)      = env
    item env (Redefine _ _)      = env	-- revisit this
    item env (Annotation _)      = env
    item env (Simple st)         = simple env st
    item env (Complex ct)        = complex env ct
    item env (SchemaElement e)   = elementDecl env e
    item env (SchemaAttribute a) = attributeDecl env a
    item env (AttributeGroup g)  = attrGroup env g
    item env (SchemaGroup g)     = group env g

    simple env s@(Restricted _ (Just n) _ _)
                                 = env{env_type=Map.insert (mkN n) (Left s)
                                                           (env_type env)}
    simple env s@(ListOf _ (Just n) _ _)
                                 = env{env_type=Map.insert (mkN n) (Left s)
                                                           (env_type env)}
    simple env s@(UnionOf _ (Just n) _ _ _)
                                 = env{env_type=Map.insert (mkN n) (Left s)
                                                           (env_type env)}
    simple env   _               = env

    -- Only toplevel names have global scope.
    -- Should we lift local names to toplevel with prefixed names?
    -- Or thread the environment explicitly through every tree-walker?
    -- Or resolve every reference to its referent in a single resolution pass?
    -- (Latter not good, because it potentially duplicates exprs?)
    complex env c
      | Nothing <- complex_name c = env
      | Just n  <- complex_name c =
              either (const id)
                     (\extn env->
                        env{env_extendty = Map.insertWith (++)
                                               (extension_base extn)
                                               [(mkN n, fp)]
                                               (env_extendty env)})
                     (isExtn (complex_content c))
              $ (if complex_abstract c then \env->
              -- because an abstract type might have no concrete instantiations!
                        env{env_extendty = Map.insertWith (++)
                                               (mkN n)
                                               []
                                               (env_extendty env)}
                 else id)
              $ env{env_type=Map.insert (mkN n) (Right c) (env_type env)
                   ,env_typeloc=Map.insert (mkN n) fp (env_typeloc env)}
          where isExtn x@SimpleContent{}  = ci_stuff x
                isExtn x@ComplexContent{} = ci_stuff x
                isExtn x@ThisType{}       = Left undefined
{-
      | Nothing <- complex_name c = env
      | Right extn <- isExtn $ complex_content c
      , Just n  <- complex_name c = env{env_extendty =
                                            Map.insertWith (++)
                                                (extension_base extn)
                                                [(mkN n, isFwd)]
                                                (env_extendty env)
                                       ,env_type=Map.insert (mkN n) (Right c)
                                                            (env_type env)}
      | Just n  <- complex_name c = env{env_type=Map.insert (mkN n) (Right c)
                                                            (env_type env)}
          where isExtn x@SimpleContent{}  = ci_stuff x
                isExtn x@ComplexContent{} = ci_stuff x
                isExtn x@ThisType{}       = Left undefined
                isFwd = case Map.lookup (extension_base extn) (env_typeloc env) of
                          Nothing  -> error $ "unknown supertype of "++show c
                          Just mod -> mod /= fp
-}
    elementDecl env e
      | Right r <- elem_nameOrRef e = env
--    | Just sg <- elem_substGroup e
--    , Left nt <- elem_nameOrRef e = env{env_substGrp=Map.insertWith (++) sg
--                                                [(mkN $ theName nt, isFwd sg)]
--                                                        (env_substGrp env)
--                                       ,env_element=Map.insert
--                                                        (mkN $ theName nt) e
--                                                        (env_element env)}
      | Left nt <- elem_nameOrRef e =
              maybe id (\sg env-> env{env_substGrp=Map.insertWith (++) sg
                                          [(mkN $ theName nt, fp)]
                                          (env_substGrp env)})
                    (elem_substGroup e)
              $ env{env_element=Map.insert (mkN $ theName nt) e
                                           (env_element env)
                   ,env_typeloc=Map.insert (mkN $ theName nt) fp
                                           (env_typeloc env)}
    attributeDecl env a
      | Right r <- attr_nameOrRef a = env
      | Left nt <- attr_nameOrRef a = env{env_attribute=
                                            Map.insert (mkN $ theName nt) a
                                                       (env_attribute env)}
    attrGroup env g
      | Right r <- attrgroup_nameOrRef g = env
      | Left n  <- attrgroup_nameOrRef g = env{env_attrgroup=Map.insert
                                                           (mkN n) g
                                                           (env_attrgroup env)}
    group env g
      | Right r <- group_nameOrRef g = env
      | Left n  <- group_nameOrRef g = env{env_group=Map.insert (mkN n) g
                                                           (env_group env)}
    mkN = N . last . wordsBy (==':')

    addNS env nss = env{env_namespace = foldr newNS (env_namespace env) nss}
              where newNS ns env = Map.insert (nsURI ns) (nsPrefix ns) env

-- | Find all direct module dependencies.
gatherImports :: Schema -> [(FilePath, Maybe String)]
gatherImports s =
    [ (f,Nothing)  | (Include f _)    <- schema_items s ] ++
    [ (f,ns)       | (Import uri f _) <- schema_items s
                   , let ns = targetPrefix (Just uri) (schema_namespaces s) ]

