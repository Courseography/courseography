-- | Validate a document against a dtd.
module Text.XML.HaXml.Validate
  ( validate
  , partialValidate
  ) where

import Prelude hiding (elem,rem,mod,sequence)
import qualified Prelude (elem)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces
import Text.XML.HaXml.Combinators (multi,tag,iffind,literal,none,o)
import Text.XML.HaXml.XmlContent (attr2str)
import Data.Maybe (fromMaybe,isNothing,fromJust)
import Data.List (intersperse,nub,(\\))
import Data.Char (isSpace)

#if __GLASGOW_HASKELL__ >= 604 || __NHC__ >= 118 || defined(__HUGS__)
-- emulate older finite map interface using Data.Map, if it is available
import qualified Data.Map as Map
type FiniteMap a b = Map.Map a b
listToFM :: Ord a => [(a,b)] -> FiniteMap a b
listToFM = Map.fromList
lookupFM :: Ord a => FiniteMap a b -> a -> Maybe b
lookupFM = flip Map.lookup
#elif __GLASGOW_HASKELL__ >= 504 || __NHC__ > 114
-- real finite map, if it is available
import Data.FiniteMap
#else
-- otherwise, a very simple and inefficient implementation of a finite map
type FiniteMap a b = [(a,b)]
listToFM :: Eq a => [(a,b)] -> FiniteMap a b
listToFM = id
lookupFM :: Eq a => FiniteMap a b -> a -> Maybe b
lookupFM fm k = lookup k fm
#endif

-- gather appropriate information out of the DTD
data SimpleDTD = SimpleDTD
    { elements   :: FiniteMap QName ContentSpec	-- content model of elem
    , attributes :: FiniteMap (QName,QName) AttType -- type of (elem,attr)
    , required   :: FiniteMap QName [QName]	-- required attributes of elem
    , ids        :: [(QName,QName)]	-- all (element,attr) with ID type
    , idrefs     :: [(QName,QName)]	-- all (element,attr) with IDREF type
    }

simplifyDTD :: DocTypeDecl -> SimpleDTD
simplifyDTD (DTD _ _ decls) =
    SimpleDTD
      { elements   = listToFM [ (name,content)
                              | Element (ElementDecl name content) <- decls ]
      , attributes = listToFM [ ((elem,attr),typ)
                              | AttList (AttListDecl elem attdefs) <- decls
                              , AttDef attr typ _ <- attdefs ]
      -- Be sure to look at all attribute declarations for each
      -- element, since we must merge them.  This implements the
      -- specification in that regard only; the specification's rules
      -- about how to merge multiple declarations for the same
      -- attribute are not considered by this implementation.
      -- See: http://www.w3.org/TR/REC-xml/#NT-AttlistDecl
      , required   = listToFM [ (elem, concat [ [ attr | AttDef attr _ REQUIRED <- attdefs ]
                                              | AttList (AttListDecl elem' attdefs) <- decls
                                              , elem' == elem ]
                                )
                              | Element (ElementDecl elem _) <- decls ]
      , ids        = [ (elem,attr)
                     | Element (ElementDecl elem _) <- decls
                     , AttList (AttListDecl name attdefs) <- decls
                     , elem == name
                     , AttDef attr (TokenizedType ID) _ <- attdefs ]
      , idrefs     = []	-- not implemented
      }

-- simple auxiliary to avoid lots of if-then-else with empty else clauses.
gives :: Bool -> a -> [a]
True `gives` x = [x]
False `gives` _ = []

-- | 'validate' takes a DTD and a tagged element, and returns a list of
--   errors in the document with respect to its DTD.
--
--   If you have several documents to validate against a single DTD,
--   then you will gain efficiency by freezing-in the DTD through partial
--   application, e.g. @checkMyDTD = validate myDTD@.
validate :: DocTypeDecl -> Element i -> [String]
validate dtd' elem = root dtd' elem ++ partialValidate dtd' elem
  where
    root (DTD name _ _) (Elem name' _ _) =
        (name/=name') `gives` ("Document type should be <"++qname name
                               ++"> but appears to be <"++qname name'++">.")

-- | 'partialValidate' is like validate, except that it does not check that
--   the element type matches that of the DTD's root element.
partialValidate :: DocTypeDecl -> Element i -> [String]
partialValidate dtd' elem = valid elem ++ checkIDs elem
  where
    dtd = simplifyDTD dtd'

    valid (Elem name attrs contents) =
        -- is the element defined in the DTD?
        let spec = lookupFM (elements dtd) name in 
        (isNothing spec) `gives` ("Element <"++qname name++"> not known.")
        -- is each attribute mentioned only once?
        ++ (let dups = duplicates (map (qname . fst) attrs) in
            not (null dups) `gives`
               ("Element <"++qname name++"> has duplicate attributes: "
                ++concat (intersperse "," dups)++"."))
        -- does each attribute belong to this element?  value is in range?
        ++ concatMap (checkAttr name) attrs
        -- are all required attributes present?
        ++ concatMap (checkRequired name attrs)
                     (fromMaybe [] (lookupFM (required dtd) name))
        -- are its children in a permissible sequence?
        ++ checkContentSpec name (fromMaybe ANY spec) contents
        -- now recursively check the element children
        ++ concatMap valid [ elm | CElem elm _ <- contents ]

    checkAttr elm (attr, val) =
        let typ = lookupFM (attributes dtd) (elm,attr)
            attval = attr2str val in
        if isNothing typ then ["Attribute \""++qname attr
                               ++"\" not known for element <"++qname elm++">."]
        else
          case fromJust typ of
            EnumeratedType e ->
              case e of
                Enumeration es ->
                    (not (attval `Prelude.elem` es)) `gives`
                          ("Value \""++attval++"\" of attribute \""
                           ++qname attr++"\" in element <"++qname elm
                           ++"> is not in the required enumeration range: "
                           ++unwords es)
                _ -> []
            _ -> []

    checkRequired elm attrs req =
        (not (req `Prelude.elem` map fst attrs)) `gives`
            ("Element <"++qname elm++"> requires the attribute \""++qname req
             ++"\" but it is missing.")

    checkContentSpec _elm ANY   _     = []
    checkContentSpec _elm EMPTY []    = []
    checkContentSpec  elm EMPTY (_:_) =
        ["Element <"++qname elm++"> is not empty but should be."]
    checkContentSpec  elm (Mixed PCDATA) cs = concatMap (checkMixed elm []) cs
    checkContentSpec  elm (Mixed (PCDATAplus names)) cs =
        concatMap (checkMixed elm names) cs
    checkContentSpec  elm (ContentSpec cp) cs = excludeText elm cs ++
        (let (errs,rest) = checkCP elm cp (flatten cs) in
         case rest of [] -> errs
                      _  -> errs++["Element <"++qname elm++"> contains extra "
                                  ++"elements beyond its content spec."])

    checkMixed  elm  permitted (CElem (Elem name _ _) _)
        | not (name `Prelude.elem` permitted) =
            ["Element <"++qname elm++"> contains an element <"++qname name
             ++"> but should not."]
    checkMixed _elm _permitted _ = []

    flatten (CElem (Elem name _ _) _: cs) = name: flatten cs
    flatten (_: cs)                       = flatten cs
    flatten []                            = []

    excludeText  elm (CElem _ _: cs) = excludeText elm cs
    excludeText  elm (CMisc _ _: cs) = excludeText elm cs
    excludeText  elm (CString _ s _: cs) | all isSpace s = excludeText elm cs
    excludeText  elm (_:_) =
        ["Element <"++qname elm++"> contains text/references but should not."]
    excludeText _elm [] = []

    -- This is a little parser really.  Returns any errors, plus the remainder
    -- of the input string.
    checkCP :: QName -> CP -> [QName] -> ([String],[QName])
    checkCP elm cp@(TagName _ None) []       = (cpError elm cp, [])
    checkCP elm cp@(TagName n None) (n':ns)
                                 | n==n'     = ([], ns)
                                 | otherwise = (cpError elm cp, n':ns)
    checkCP  _     (TagName _ Query) []      = ([],[])
    checkCP  _     (TagName n Query) (n':ns)
                                 | n==n'     = ([], ns)
                                 | otherwise = ([], n':ns)
    checkCP  _     (TagName _ Star) []       = ([],[])
    checkCP elm    (TagName n Star) (n':ns)
                                 | n==n'     = checkCP elm (TagName n Star) ns
                                 | otherwise = ([], n':ns)
    checkCP elm cp@(TagName _ Plus) []       = (cpError elm cp, [])
    checkCP elm cp@(TagName n Plus) (n':ns)
                                 | n==n'     = checkCP elm (TagName n Star) ns
                                 | otherwise = (cpError elm cp, n':ns)
 -- omit this clause, to permit (a?|b?) as a valid but empty choice
 -- checkCP elem cp@(Choice cps None) [] = (cpError elem cp, [])
    checkCP elm cp@(Choice cps None) ns =
        let next = choice elm ns cps in
        if null next then (cpError elm cp, ns)
        else ([], head next)	-- choose the first alternative with no errors
    checkCP _      (Choice _   Query) [] = ([],[])
    checkCP elm    (Choice cps Query) ns =
        let next = choice elm ns cps in
        if null next then ([],ns)
        else ([], head next)
    checkCP _      (Choice _   Star) [] = ([],[])
    checkCP elm    (Choice cps Star) ns =
        let next = choice elm ns cps in
        if null next then ([],ns)
        else checkCP elm (Choice cps Star) (head next)
    checkCP elm cp@(Choice _   Plus) [] = (cpError elm cp, [])
    checkCP elm cp@(Choice cps Plus) ns =
        let next = choice elm ns cps in
        if null next then (cpError elm cp, ns)
        else checkCP elm (Choice cps Star) (head next)
 -- omit this clause, to permit (a?,b?) as a valid but empty sequence
 -- checkCP elem cp@(Seq cps None) [] = (cpError elem cp, [])
    checkCP elm cp@(Seq cps None) ns =
        let (errs,next) = sequence elm ns cps in
        if null errs then ([],next)
        else (cpError elm cp++errs, ns)
    checkCP _      (Seq _   Query) [] = ([],[])
    checkCP elm    (Seq cps Query) ns =
        let (errs,next) = sequence elm ns cps in
        if null errs then ([],next)
        else ([], ns)
    checkCP _      (Seq _   Star) [] = ([],[])
    checkCP elm    (Seq cps Star) ns =
        let (errs,next) = sequence elm ns cps in
        if null errs then checkCP elm (Seq cps Star) next
        else ([], ns)
    checkCP elm cp@(Seq _   Plus) [] = (cpError elm cp, [])
    checkCP elm cp@(Seq cps Plus) ns =
        let (errs,next) = sequence elm ns cps in
        if null errs then checkCP elm (Seq cps Star) next
        else (cpError elm cp++errs, ns)

    choice elm ns cps =  -- return only those parses that don't give any errors
        [ rem | ([],rem) <- map (\cp-> checkCP elm (definite cp) ns) cps ]
        ++ [ ns | all possEmpty cps ]
        where definite (TagName n Query)  = TagName n None
              definite (Choice cps Query) = Choice cps None
              definite (Seq cps Query)    = Seq cps None
              definite (TagName n Star)   = TagName n Plus
              definite (Choice cps Star)  = Choice cps Plus
              definite (Seq cps Star)     = Seq cps Plus
              definite x                  = x
              possEmpty (TagName _ mod)   = mod `Prelude.elem` [Query,Star]
              possEmpty (Choice cps None) = all possEmpty cps
              possEmpty (Choice _ mod)    = mod `Prelude.elem` [Query,Star]
              possEmpty (Seq cps None)    = all possEmpty cps
              possEmpty (Seq _ mod)       = mod `Prelude.elem` [Query,Star]
    sequence elm ns cps =  -- accumulate errors down the sequence
        foldl (\(es,ns) cp-> let (es',ns') = checkCP elm cp ns
                             in (es++es', ns'))
              ([],ns) cps

    checkIDs elm =
        let celem = CElem elm undefined
            showAttr a = iffind (printableName a) literal none
            idElems = concatMap (\(name, at)->
                                     multi (showAttr at `o`
                                                tag (printableName name))
                                           celem)
                                (ids dtd)
            badIds  = duplicates (map (\(CString _ s _)->s) idElems)
        in not (null badIds) `gives`
               ("These attribute values of type ID are not unique: "
                ++concat (intersperse "," badIds)++".")


cpError :: QName -> CP -> [String]
cpError elm cp =
    ["Element <"++qname elm++"> should contain "++display cp++" but does not."]


display :: CP -> String
display (TagName name mod) = qname name ++ modifier mod
display (Choice cps mod)   = "(" ++ concat (intersperse "|" (map display cps))
                             ++ ")" ++ modifier mod
display (Seq cps mod)      = "(" ++ concat (intersperse "," (map display cps))
                             ++ ")" ++ modifier mod

modifier :: Modifier -> String
modifier None  = ""
modifier Query = "?"
modifier Star  = "*"
modifier Plus  = "+"

duplicates :: Eq a => [a] -> [a]
duplicates xs = xs \\ (nub xs)

qname :: QName -> String
qname n = printableName n
