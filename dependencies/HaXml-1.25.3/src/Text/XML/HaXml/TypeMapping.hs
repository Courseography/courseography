module Text.XML.HaXml.TypeMapping
  (
  -- * A class to get an explicit type representation for any value
    HTypeable(..)	-- sole method, toHType
  -- * Explicit representation of Haskell datatype information
  , HType(..)		-- instance of Eq, Show
  , Constr(..)		-- instance of Eq, Show
  -- * Helper functions to extract type info as strings
  , showHType		-- :: HType -> ShowS
  , showConstr		-- :: Int -> HType -> String
  -- * Conversion from Haskell datatype to DTD
  , toDTD
  ) where

import Text.XML.HaXml.Types
import Data.List (partition, intersperse)
import Text.PrettyPrint.HughesPJ (render)
import qualified Text.XML.HaXml.Pretty as PP


------------------------------------------------------------------------
        -- idea: in DrIFT,
        --      named field == primitive type, becomes an attribute
        --      named field == single-constructor type, renames the tag
        --      named field == multi-constructor type, as normal
        -- if prefix of all named fields is roughly typename, delete it

-- | @HTypeable@ promises that we can create an explicit representation of
--   of the type of any value.
class HTypeable a where
    toHType :: a -> HType

-- | A concrete representation of any Haskell type.
data HType =
      Maybe HType
    | List HType
    | Tuple [HType]
    | Prim String String        -- ^ separate Haskell name and XML name
    | String
    | Defined String [HType] [Constr]
	-- ^ A user-defined type has a name, a sequence of type variables,
	--   and a set of constructors.  (The variables might already be
	--   instantiated to actual types.)
    deriving (Show)

instance Eq HType where
    (Maybe x)  == (Maybe y)  =  x==y
    (List x)   == (List y)   =  x==y
    (Tuple xs) == (Tuple ys) =  xs==ys
    (Prim x _) == (Prim y _) =  x==y
    String     == String     =  True
    (Defined n _xs _) == (Defined m _ys _)  =  n==m 	-- && xs==ys
    _          == _          =  False

-- | A concrete representation of any user-defined Haskell constructor.
--   The constructor has a name, and a sequence of component types.  The
--   first sequence of types represents the minimum set of free type
--   variables occurring in the (second) list of real component types.
--   If there are fieldnames, they are contained in the final list, and
--   correspond one-to-one with the component types.
data Constr = Constr String [HType] [HType] -- (Maybe [String])
    deriving (Eq,Show)

-- | Project the n'th constructor from an HType and convert it to a string
--   suitable for an XML tagname.
showConstr :: Int -> HType -> String
showConstr n (Defined _ _ cs) = flatConstr (cs!!n) ""
showConstr _ _ = error "no constructors for builtin types"

------------------------------------------------------------------------
-- Some instances
instance HTypeable Bool where
    toHType   _    = Prim "Bool" "bool"
instance HTypeable Int where
    toHType   _    = Prim "Int" "int"
instance HTypeable Integer where
    toHType   _    = Prim "Integer" "integer"
instance HTypeable Float where
    toHType   _    = Prim "Float" "float"
instance HTypeable Double where
    toHType   _    = Prim "Double" "double"
instance HTypeable Char where
    toHType   _    = Prim "Char" "char"

instance HTypeable () where
    toHType _      = Prim "unit" "unit"
instance (HTypeable a, HTypeable b) => HTypeable (a,b) where
    toHType p      = Tuple [toHType a, toHType b]
                   where  (a,b) = p
instance (HTypeable a, HTypeable b, HTypeable c) => HTypeable (a,b,c) where
    toHType p      = Tuple [toHType a, toHType b, toHType c]
                   where  (a,b,c) = p
instance (HTypeable a, HTypeable b, HTypeable c, HTypeable d) =>
         HTypeable (a,b,c,d) where
    toHType p      = Tuple [toHType a, toHType b, toHType c, toHType d]
                   where  (a,b,c,d) = p
instance (HTypeable a, HTypeable b, HTypeable c, HTypeable d, HTypeable e) =>
         HTypeable (a,b,c,d,e) where
    toHType p      = Tuple [ toHType a, toHType b, toHType c, toHType d
                           , toHType e ]
                   where  (a,b,c,d,e) = p
instance ( HTypeable a, HTypeable b, HTypeable c, HTypeable d, HTypeable e
         , HTypeable f) =>
         HTypeable (a,b,c,d,e,f) where
    toHType p      = Tuple [ toHType a, toHType b, toHType c, toHType d
                           , toHType e, toHType f ]
                   where  (a,b,c,d,e,f) = p
instance ( HTypeable a, HTypeable b, HTypeable c, HTypeable d, HTypeable e
         , HTypeable f, HTypeable g) =>
         HTypeable (a,b,c,d,e,f,g) where
    toHType p      = Tuple [ toHType a, toHType b, toHType c, toHType d
                           , toHType e, toHType f, toHType g ]
                   where  (a,b,c,d,e,f,g) = p
instance ( HTypeable a, HTypeable b, HTypeable c, HTypeable d, HTypeable e
         , HTypeable f, HTypeable g, HTypeable h) =>
         HTypeable (a,b,c,d,e,f,g,h) where
    toHType p      = Tuple [ toHType a, toHType b, toHType c, toHType d
                           , toHType e, toHType f, toHType g, toHType h ]
                   where  (a,b,c,d,e,f,g,h) = p
instance ( HTypeable a, HTypeable b, HTypeable c, HTypeable d, HTypeable e
         , HTypeable f, HTypeable g, HTypeable h, HTypeable i) =>
         HTypeable (a,b,c,d,e,f,g,h,i) where
    toHType p      = Tuple [ toHType a, toHType b, toHType c, toHType d
                           , toHType e, toHType f, toHType g, toHType h
                           , toHType i ]
                   where  (a,b,c,d,e,f,g,h,i) = p
instance ( HTypeable a, HTypeable b, HTypeable c, HTypeable d, HTypeable e
         , HTypeable f, HTypeable g, HTypeable h, HTypeable i, HTypeable j) =>
         HTypeable (a,b,c,d,e,f,g,h,i,j) where
    toHType p      = Tuple [ toHType a, toHType b, toHType c, toHType d
                           , toHType e, toHType f, toHType g, toHType h
                           , toHType i, toHType j ]
                   where  (a,b,c,d,e,f,g,h,i,j) = p
instance ( HTypeable a, HTypeable b, HTypeable c, HTypeable d, HTypeable e
         , HTypeable f, HTypeable g, HTypeable h, HTypeable i, HTypeable j
         , HTypeable k) =>
         HTypeable (a,b,c,d,e,f,g,h,i,j,k) where
    toHType p      = Tuple [ toHType a, toHType b, toHType c, toHType d
                           , toHType e, toHType f, toHType g, toHType h
                           , toHType i, toHType j, toHType k ]
                   where  (a,b,c,d,e,f,g,h,i,j,k) = p
instance ( HTypeable a, HTypeable b, HTypeable c, HTypeable d, HTypeable e
         , HTypeable f, HTypeable g, HTypeable h, HTypeable i, HTypeable j
         , HTypeable k, HTypeable l) =>
         HTypeable (a,b,c,d,e,f,g,h,i,j,k,l) where
    toHType p      = Tuple [ toHType a, toHType b, toHType c, toHType d
                           , toHType e, toHType f, toHType g, toHType h
                           , toHType i, toHType j, toHType k, toHType l ]
                   where  (a,b,c,d,e,f,g,h,i,j,k,l) = p
instance ( HTypeable a, HTypeable b, HTypeable c, HTypeable d, HTypeable e
         , HTypeable f, HTypeable g, HTypeable h, HTypeable i, HTypeable j
         , HTypeable k, HTypeable l, HTypeable m) =>
         HTypeable (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    toHType p      = Tuple [ toHType a, toHType b, toHType c, toHType d
                           , toHType e, toHType f, toHType g, toHType h
                           , toHType i, toHType j, toHType k, toHType l
                           , toHType m ]
                   where  (a,b,c,d,e,f,g,h,i,j,k,l,m) = p
instance ( HTypeable a, HTypeable b, HTypeable c, HTypeable d, HTypeable e
         , HTypeable f, HTypeable g, HTypeable h, HTypeable i, HTypeable j
         , HTypeable k, HTypeable l, HTypeable m, HTypeable n) =>
         HTypeable (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    toHType p      = Tuple [ toHType a, toHType b, toHType c, toHType d
                           , toHType e, toHType f, toHType g, toHType h
                           , toHType i, toHType j, toHType k, toHType l
                           , toHType m, toHType n ]
                   where  (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = p
instance ( HTypeable a, HTypeable b, HTypeable c, HTypeable d, HTypeable e
         , HTypeable f, HTypeable g, HTypeable h, HTypeable i, HTypeable j
         , HTypeable k, HTypeable l, HTypeable m, HTypeable n, HTypeable o) =>
         HTypeable (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    toHType p      = Tuple [ toHType a, toHType b, toHType c, toHType d
                           , toHType e, toHType f, toHType g, toHType h
                           , toHType i, toHType j, toHType k, toHType l
                           , toHType m, toHType n, toHType o ]
                   where  (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = p

instance (HTypeable a) => HTypeable (Maybe a) where
    toHType m      = Maybe (toHType x)   where   (Just x) = m
instance (HTypeable a, HTypeable b) => HTypeable (Either a b) where
    toHType m      = Defined "Either" [hx, hy]
                         [ Constr "Left" [hx] [hx] {-Nothing-}
                         , Constr "Right" [hy] [hy] {-Nothing-}]
                   where (Left x)  = m
                         (Right y) = m
                         hx = toHType x
                         hy = toHType y

instance HTypeable a => HTypeable [a] where
    toHType xs     = case toHType x of (Prim "Char" _) -> String
                                       _ -> List (toHType x)
                   where  (x:_) = xs

------------------------------------------------------------------------

-- | 'toDTD' converts a concrete representation of the Haskell type of
--   a value (obtained by the method 'toHType') into a real DocTypeDecl.
--   It ensures that PERefs are defined before they are used, and that no
--   element or attribute-list is declared more than once.
toDTD :: HType -> DocTypeDecl
toDTD ht =
  DTD (toplevel ht) Nothing (macrosFirst (reverse (h2d True [] [] [ht])))
  where
    macrosFirst :: [MarkupDecl] -> [MarkupDecl]
    macrosFirst decls = concat [p, p'] where (p, p') = partition f decls
                                             f (Entity _) = True
                                             f _ = False
    toplevel ht@(Defined _ _ _) = N $ showHType ht "-XML"
    toplevel ht@_               = N $ showHType ht ""
    c0 = False
    h2d :: Bool -> [HType] -> [Constr] -> [HType] -> [MarkupDecl]
    -- toplevel?   history    history   remainingwork     result
    h2d _c _history _chist []       = []
    h2d  c  history  chist (ht:hts) =
      if ht `elem` history then h2d c0 history chist hts
      else
        case ht of
          Maybe ht0  -> declelem ht: h2d c0 (ht:history) chist (ht0:hts)
          List ht0   -> declelem ht: h2d c0 (ht:history) chist (ht0:hts)
          Tuple hts0 -> (c ? (declelem ht:))
                                     (h2d c0 history chist (hts0++hts))
          Prim _ _   -> declprim ht ++ h2d c0 (ht:history) chist hts
          String     -> declstring:    h2d c0 (ht:history) chist hts
          Defined _ _ cs ->
               let hts0 = concatMap grab cs in
               (c ? (decltopelem ht:)) (declmacro ht chist)
               ++ h2d c0 (ht:history) (cs++chist) (hts0++hts)
    declelem ht =
      Element (ElementDecl (N $ showHType ht "")
                           (ContentSpec (outerHtExpr ht)))
    decltopelem ht =    -- hack to avoid peref at toplevel
      Element (ElementDecl (N $ showHType ht "-XML")
                           (ContentSpec (innerHtExpr ht None)))
    declmacro ht@(Defined _ _ cs) chist =
      Entity (EntityPEDecl (PEDecl (showHType ht "") (PEDefEntityValue ev))):
      concatMap (declConstr chist) cs
      where ev = EntityValue [EVString (render (PP.cp (outerHtExpr ht)))]
    declConstr chist c@(Constr s fv hts)
      | c `notElem` chist = [Element (ElementDecl (N $ flatConstr c "")
                                         (ContentSpec (constrHtExpr c)))]
      | otherwise = [] 
    declprim (Prim _ t) =
      [ Element (ElementDecl (N t) EMPTY)
      , AttList (AttListDecl (N t) [AttDef (N "value") StringType REQUIRED])]
    declstring =
      Element (ElementDecl (N "string") (Mixed PCDATA))
    grab (Constr _ _ hts) = hts

(?) :: Bool -> (a->a) -> (a->a)
b ? f | b     = f
      | not b = id

-- Flatten an HType to a String suitable for an XML tagname.
showHType :: HType -> ShowS
showHType (Maybe ht)  = showString "maybe-" . showHType ht
showHType (List ht)   = showString "list-" . showHType ht
showHType (Tuple hts) = showString "tuple" . shows (length hts)
                        . showChar '-'
                        . foldr1 (.) (intersperse (showChar '-')
                                                  (map showHType hts))
showHType (Prim _ t)  = showString t
showHType String      = showString "string"
showHType (Defined s fv _)
                      = showString s . ((length fv > 0) ? (showChar '-'))
                        . foldr (.) id (intersperse (showChar '-')
                                                    (map showHType fv))

flatConstr :: Constr -> ShowS
flatConstr (Constr s fv _)
        = showString s . ((length fv > 0) ? (showChar '-'))
          . foldr (.) id (intersperse (showChar '-') (map showHType fv))

outerHtExpr :: HType -> CP
outerHtExpr (Maybe ht)      = innerHtExpr ht Query
outerHtExpr (List ht)       = innerHtExpr ht Star
outerHtExpr (Defined _s _fv cs) =
    Choice (map (\c->TagName (N $ flatConstr c "") None) cs) None
outerHtExpr ht              = innerHtExpr ht None

innerHtExpr :: HType -> Modifier -> CP
innerHtExpr (Prim _ t)  m = TagName (N t) m
innerHtExpr (Tuple hts) m = Seq (map (\c-> innerHtExpr c None) hts) m
innerHtExpr ht@(Defined _ _ _) m = -- CPPE (showHType ht "") (outerHtExpr ht)
                                   TagName (N ('%': showHType ht ";")) m
                                                        --  ***HACK!!!***
innerHtExpr ht m = TagName (N $ showHType ht "") m

constrHtExpr :: Constr -> CP
constrHtExpr (Constr _s _fv [])  = TagName (N "EMPTY") None   --  ***HACK!!!***
constrHtExpr (Constr _s _fv hts) = innerHtExpr (Tuple hts) None

------------------------------------------------------------------------
