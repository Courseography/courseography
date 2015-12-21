{-# LANGUAGE PatternGuards #-}
module Text.XML.HaXml.Namespaces
  ( nullNamespace
  , expandedName
  , namespaceName
  , localName
  , printableName
  , qualify
  , deQualify
  , qualifyExceptLocal
  , initNamespaceEnv
  , augmentNamespaceEnv
  , resolveAllNames
  ) where

import Prelude hiding (lookup)
import Text.XML.HaXml.Types
import Data.Map as Map (Map, insert, lookup, empty)
import Data.List (isPrefixOf)

-- | The null Namespace (no prefix, no URI).
nullNamespace :: Namespace
nullNamespace  = Namespace { nsPrefix="", nsURI="" }

-- | Every Name can be split into a Namespace and local Name.  The Namespace
--   might of course be missing.
expandedName   :: QName -> (Maybe Namespace, String)
expandedName n  = (namespaceName n, localName n)

-- | Return the (possibly absent) Namespace component of a Name.
namespaceName          :: QName -> Maybe Namespace
namespaceName (N _)     = Nothing
namespaceName (QN ns _) = Just ns

-- | Return the local component of a Name, without its Namespace.
localName          :: QName -> String
--localName (N n)     | ':'`elem`n = tail $ dropWhile (/=':') n
localName (N n)     = n
localName (QN _ n)  = n

-- | Return the printable string for a Name, i.e. attaching a prefix
--   for its namespace (if it has one).
printableName :: QName -> String
printableName (N n)     = n
printableName (QN ns n) | null (nsPrefix ns) = n
                        | otherwise          = nsPrefix ns++':':n

-- | 'qualify' splits a Name of the form "pr:nm" into the
--   prefix "pr" and local name "nm", and looks up the prefix in the
--   given environment to determine its Namespace.  There may also be a
--   default namespace (the first argument) for unqualified names.
--   In the absence of a default Namespace, a Name that does not have
--   a prefix remains unqualified.  A prefix that is not known in the
--   environment becomes a fresh namespace with null URI.  A Name that is
--   already qualified is passed unchanged, unless its URI was null, in
--   which case we check afresh for that prefix in the environment.
qualify :: Maybe Namespace -> Map String Namespace -> QName -> QName
qualify def env (N n)
        | ':'`elem`n      = let (pre,':':nm) = span (/=':') n in
                            QN (maybe nullNamespace{nsPrefix=pre} id
                                      (Map.lookup pre env))
                               nm
        | Just d <- def   = QN d n
        | otherwise       = N n
qualify _ env qn@(QN ns n)
        | null (nsURI ns) = QN (maybe ns id (Map.lookup (nsPrefix ns) env)) n
        | otherwise       = qn

-- | 'deQualify' has the same signature as 'qualify', but ignores the
--   arguments for default namespace and environment, and simply removes any
--   pre-existing qualification.
deQualify :: Maybe Namespace -> Map String Namespace -> QName -> QName
deQualify _ _ (QN _ n) = N n
deQualify _ _ (N n)    = N n

-- | 'qualifyExceptLocal' converts names to qualified names, except where
--   an existing qualification matches the default namespace, in which case
--   the qualification is removed.  (This is useful when translating QNames
--   to Haskell, because Haskell qualified names cannot use the current
--   module name.)
qualifyExceptLocal :: Maybe Namespace -> Map String Namespace -> QName -> QName
qualifyExceptLocal Nothing    env  qn   = qualify Nothing env qn
qualifyExceptLocal (Just def) env (N n)
        | ':'`elem`n      = let (pre,':':nm) = span (/=':') n in
                            if nsPrefix def == pre then N nm
                            else QN (maybe nullNamespace{nsPrefix=pre} id
                                          (Map.lookup pre env))
                                    nm
        | otherwise       = N n
qualifyExceptLocal (Just def) env qn@(QN ns n)
        | def==ns         = N n
        | null (nsURI ns) = QN (maybe ns id (Map.lookup (nsPrefix ns) env)) n
        | otherwise       = qn

-- | The initial Namespace environment.  It always has bindings for the
--   prefixes 'xml' and 'xmlns'.
initNamespaceEnv :: Map String Namespace
initNamespaceEnv =
      Map.insert "xmlns" Namespace{nsPrefix="xmlns"
                                  ,nsURI="http://www.w3.org/2000/xmlns/"}
    $ Map.insert "xml"   Namespace{nsPrefix="xml"
                                  ,nsURI="http://www.w3.org/XML/1998/namespace"}
    $ Map.empty

-- | Add a fresh Namespace into the Namespace environment.  It is not
--   permitted to rebind the prefixes 'xml' or 'xmlns', but that is not
--   checked here.
augmentNamespaceEnv :: Namespace -> Map String Namespace
                                 -> Map String Namespace
augmentNamespaceEnv ns env = Map.insert (nsPrefix ns) ns env
{-
augmentNamespaceEnv :: Namespace -> Map String Namespace
                                 -> Either String (Map String Namespace)
augmentNamespaceEnv ns env
    | nsPrefix ns == "xml"   = Left "cannot rebind the 'xml' namespace"
    | nsPrefix ns == "xmlns" = Left "cannot rebind the 'xmlns' namespace"
    | otherwise              = Right (Map.insert (nsPrefix ns) ns env)
-}

-- | resolveAllNames in a document, causes every name to be properly
--   qualified with its namespace.  There is a default namespace for any
--   name that was originally unqualified.  This is likely only useful when
--   dealing with parsed document, less useful when generating a document
--   from scratch.
resolveAllNames :: (Maybe Namespace -> Map String Namespace -> QName -> QName)
                   -> Document i -> Document i
resolveAllNames qualify (Document prolog entities elm misc) =
    Document (walkProlog prolog) entities
             (walkElem Nothing initNamespaceEnv elm) misc
  where
    qualifyInDTD = qualify Nothing initNamespaceEnv
    walkProlog (Prolog xml misc0 mDTD misc1) =
                Prolog xml misc0 (maybe Nothing (Just . walkDTD) mDTD) misc1
    walkDTD (DTD qn ext mds)     = DTD (qualifyInDTD qn) ext (map walkMD mds)
    --
    walkMD (Element ed)          = Element (walkED ed)
    walkMD (AttList ald)         = AttList (walkALD ald)
    walkMD md                    = md
    --
    walkED (ElementDecl qn cs)   = ElementDecl (qualifyInDTD qn) (walkCS cs)
    --
    walkCS (ContentSpec cp)      = ContentSpec (walkCP cp)
    walkCS (Mixed m)             = Mixed (walkM m)
    walkCS cs                    = cs
    --
    walkCP (TagName qn m)        = TagName (qualifyInDTD qn) m
    walkCP cp                    = cp
    --
    walkM (PCDATAplus qns)       = PCDATAplus (map qualifyInDTD qns)
    walkM PCDATA                 = PCDATA
    --
    walkALD (AttListDecl qn ads) = AttListDecl (qualifyInDTD qn)
                                               (map walkAD ads)
    --
    walkAD (AttDef qn at dd)     = AttDef (qualifyInDTD qn) at dd
    --
    walkElem def env (Elem qn attrs conts) =
                      Elem (qualify def' env' qn)
                           (map (\ (a,v)-> (qualify Nothing env' a, v)) attrs)
                           (map (walkContent def' env') conts)
        where def' = foldr const def  -- like "maybe def head", but for lists
                           (map defNamespace (matching (=="xmlns") attrs))
              env' = foldr augmentNamespaceEnv env
                           (map mkNamespace
                                (matching ("xmlns:"`isPrefixOf`) attrs))
              defNamespace :: Attribute -> Maybe Namespace
              defNamespace (_ {-N "xmlns"-}, atv)
                      | null (show atv) = Nothing
                      | otherwise       = Just nullNamespace{nsURI=show atv}
              mkNamespace :: Attribute -> Namespace
              mkNamespace (N n, atv)  = let (_,':':nm) = span (/=':') n in 
                                        Namespace{nsPrefix=nm,nsURI=show atv}
              matching :: (String->Bool) -> [Attribute] -> [Attribute]
              matching p = filter (p . printableName . fst)
    --
    walkContent def env (CElem e i) = CElem (walkElem def env e) i
    walkContent _   _   content     = content

    -- Notes: we DO NOT CHECK some of the Namespace well-formedness conditions:
    --        Prefix Declared
    --        No Prefix Undeclaring
    --        Attributes Unique
    -- The functions defNamespace and mkNamespace are partial - they do not
    -- handle the QN case - but this is OK because they are only called from
    -- def' and env', which check the precondition
