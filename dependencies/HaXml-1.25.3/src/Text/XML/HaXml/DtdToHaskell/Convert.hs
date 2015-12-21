-- | This module performs the translation of a parsed XML DTD into the
--   internal representation of corresponding Haskell data\/newtypes.
--
--   Note that dtdToTypeDef is partial - it will crash if you resolve
--   qualified names (namespaces) to URIs beforehand.  It will only work
--   on the original literal name forms "prefix:name".

module Text.XML.HaXml.DtdToHaskell.Convert
  ( dtd2TypeDef
  ) where

import Data.List (intersperse,nub)

import Text.XML.HaXml.Types hiding (Name)
import Text.XML.HaXml.DtdToHaskell.TypeDef


---- Internal representation for database of DTD decls ----
data Record = R [AttDef] ContentSpec
-- type Db = [(QName,Record)]


---- Build a database of DTD decls then convert them to typedefs ----
---- (Done in two steps because we need to merge ELEMENT and ATTLIST decls.)
---- Apparently multiple ATTLIST decls for the same element are permitted,
---- although only one ELEMENT decl for it is allowed.
dtd2TypeDef :: [MarkupDecl] -> [TypeDef]
dtd2TypeDef mds =
  (concatMap convert . reverse . database []) mds
  where
  database db [] = db
  database db (m:ms) =
      case m of
        (Element (ElementDecl n cs)) ->
          case lookup n db of
            Nothing -> database ((n, R [] cs):db) ms
            (Just (R as _)) -> database (replace n (R as cs) db) ms
        (AttList (AttListDecl n as)) ->
          case lookup n db of
            Nothing -> database ((n, R as EMPTY):db) ms
            (Just (R a cs)) -> database (replace n (R (nub (a++as)) cs) db) ms
    --  (MarkupPE _ m') -> database db (m':ms)
        _ -> database db ms

  replace _ _ [] = error "dtd2TypeDef.replace: no element to replace"
  replace n v (x@(n0,_):db)
      | n==n0     = (n,v): db
      | otherwise = x: replace n v db



---- Convert DTD record to typedef ----
convert :: (QName, Record) -> [TypeDef]
convert (N n, R as cs) =
    case cs of
      EMPTY                   -> modifier None []
      ANY                     -> modifier None [[Any]]
                                 --error "NYI: contentspec of ANY"
      (Mixed PCDATA)          -> modifier None [[String]]
      (Mixed (PCDATAplus ns)) -> modifier Star ([StringMixed]
                                                : map ((:[]) . Defined . name
                                                       . \(N n)->n)
                                                       ns)
      (ContentSpec cp)        ->
          case cp of
            (TagName (N n') m) -> modifier m [[Defined (name n')]]
            (Choice cps m)     -> modifier m (map ((:[]).inner) cps)
            (Seq cps m)        -> modifier m [map inner cps]
    ++ concatMap (mkAttrDef (N n)) as
  where
    attrs    :: AttrFields
    attrs     = map (mkAttrField (N n)) as

    modifier None sts   = mkData sts            attrs False (name n)
    modifier m   [[st]] = mkData [[modf m st]]  attrs False (name n)
    modifier m    sts   = mkData [[modf m (Defined (name_ n))]]
                                                attrs False (name n) ++
                          mkData sts            []    True  (name_ n)

    inner :: CP -> StructType
    inner (TagName (N n') m) = modf m (Defined (name n'))
    inner (Choice cps m)     = modf m (OneOf (map inner cps))
    inner (Seq cps None)     = Tuple (map inner cps)
    inner (Seq cps m)        = modf m (Tuple (map inner cps))

    modf None x  = x
    modf Query x = Maybe x
    modf Star x  = List x
    modf Plus x  = List1 x

mkData :: [[StructType]] -> AttrFields -> Bool -> Name -> [TypeDef]
mkData []   fs aux n  = [DataDef aux n fs []]
mkData [ts] fs aux n  = [DataDef aux n fs [(n, ts)]]
mkData tss  fs aux n  = [DataDef aux n fs (map (mkConstr n) tss)]
  where
    mkConstr m ts = (mkConsName m ts, ts)
    mkConsName (Name x m) sts = Name x (m++concat (intersperse "_" (map flatten sts)))
    flatten (Maybe st)   = {-"Maybe_" ++ -} flatten st
    flatten (List st)    = {-"List_" ++ -} flatten st
    flatten (List1 st)   = {-"List1_" ++ -} flatten st
    flatten (Tuple sts)  = {-"Tuple" ++ show (length sts) ++ "_" ++ -}
                            concat (intersperse "_" (map flatten sts))
    flatten StringMixed  = "Str"
    flatten String       = "Str"
    flatten (OneOf sts)  = {-"OneOf" ++ show (length sts) ++ "_" ++ -}
                            concat (intersperse "_" (map flatten sts))
    flatten Any          = "Any"
    flatten (Defined (Name _ m))  = m

mkAttrDef :: QName -> AttDef -> [TypeDef]
mkAttrDef _ (AttDef _ StringType _) =
    []
mkAttrDef _ (AttDef _ (TokenizedType _) _) =
    [] -- mkData [[String]] [] False (name n)
mkAttrDef (N e) (AttDef (N n) (EnumeratedType (NotationType nt)) _) =
    [EnumDef (name_a e n) (map (name_ac e n) nt)]
mkAttrDef (N e) (AttDef (N n) (EnumeratedType (Enumeration es)) _) =
    [EnumDef (name_a e n) (map (name_ac e n) es)]
        -- Default attribute values not handled here

mkAttrField :: QName -> AttDef -> (Name,StructType)
mkAttrField (N e) (AttDef (N n) typ req) = (name_f e n, mkType typ req)
  where
    mkType StringType REQUIRED = String
    mkType StringType IMPLIED  = Maybe String
    mkType StringType (DefaultTo v@(AttValue _) _) = Defaultable String (show v)
    mkType (TokenizedType _) REQUIRED  = String
    mkType (TokenizedType _) IMPLIED   = Maybe String
    mkType (TokenizedType _) (DefaultTo v@(AttValue _) _) =
                                                        Defaultable String (show v)
    mkType (EnumeratedType _) REQUIRED = Defined (name_a e n)
    mkType (EnumeratedType _) IMPLIED  = Maybe (Defined (name_a e n))
    mkType (EnumeratedType _) (DefaultTo v@(AttValue _) _) =
                Defaultable (Defined (name_a e n)) (hName (name_ac e n (show v)))

