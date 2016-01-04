-- | Pretty-print the internal Haskell model of XSD datatypes to a
--   Haskell hs-boot module containing only stub type declarations.
--   This approach is intended to work around issues of mutually recursive
--   datatype definitions.
module Text.XML.HaXml.Schema.PrettyHsBoot
  ( ppComment
  , ppModule
  , ppHighLevelDecl
  , ppHighLevelDecls
  , ppvList
  ) where

import Text.XML.HaXml.Types (QName(..),Namespace(..))
import Text.XML.HaXml.Schema.HaskellTypeModel
import Text.XML.HaXml.Schema.XSDTypeModel (Occurs(..))
import Text.XML.HaXml.Schema.NameConversion
import Text.PrettyPrint.HughesPJ as PP

import Data.List (intersperse,notElem,inits)
import Data.Maybe (isJust,fromJust,catMaybes)

-- | Vertically pretty-print a list of things, with open and close brackets,
--   and separators.
ppvList :: String -> String -> String -> (a->Doc) -> [a] -> Doc
ppvList open sep close pp []     = text open <> text close
ppvList open sep close pp (x:xs) = text open <+> pp x
                                   $$ vcat (map (\y-> text sep <+> pp y) xs)
                                   $$ text close

data CommentPosition = Before | After

-- | Generate aligned haddock-style documentation.
--   (but without escapes in comment text yet)
ppComment :: CommentPosition -> Comment -> Doc
ppComment _   Nothing  = empty
ppComment pos (Just s) =
    text "--" <+> text (case pos of Before -> "|"; After -> "^") <+> text c
    $$
    vcat (map (\x-> text "--  " <+> text x) cs)
  where
    (c:cs) = lines (paragraph 60 s)

-- | Pretty-print a Haskell-style name.
ppHName :: HName -> Doc
ppHName (HName x) = text x

-- | Pretty-print an XML-style name.
ppXName :: XName -> Doc
ppXName (XName (N x))     = text x
ppXName (XName (QN ns x)) = text (nsPrefix ns) <> text ":" <> text x

-- | Some different ways of using a Haskell identifier.
ppModId, ppConId, ppVarId, ppUnqConId, ppUnqVarId, ppFwdConId
    :: NameConverter -> XName -> Doc
ppModId nx = ppHName . modid nx
ppConId nx = ppHName . conid nx
ppVarId nx = ppHName . varid nx
ppUnqConId nx = ppHName . unqconid nx
ppUnqVarId nx = ppHName . unqvarid nx
ppFwdConId nx = ppHName . fwdconid nx

ppJoinConId, ppFieldId :: NameConverter -> XName -> XName -> Doc
ppJoinConId nx p q = ppHName (conid nx p) <> text "_" <> ppHName (conid nx q)
ppFieldId   nx     = \t-> ppHName . fieldid nx t

-- | Convert a whole document from HaskellTypeModel to Haskell source text.
ppModule :: NameConverter -> Module -> Doc
ppModule nx m =
    text "{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}"
    $$ text "{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}"
    $$ text "module" <+> ppModId nx (module_name m)
    $$ nest 2 (text "( module" <+> ppModId nx (module_name m)
              $$ vcat (map (\(XSDInclude ex com)->
                               ppComment Before com
                               $$ text ", module" <+> ppModId nx ex)
                           (module_re_exports m))
              $$ text ") where")
    $$ text " "
    $$ text "import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))"
    $$ text "import Text.XML.HaXml.Schema.Schema as Schema"
    $$ (case module_xsd_ns m of
         Nothing -> text "import Text.XML.HaXml.Schema.PrimitiveTypes as Xsd"
         Just ns -> text "import qualified Text.XML.HaXml.Schema.PrimitiveTypes as"<+>ppConId nx ns)
    $$ vcat (map (ppHighLevelDecl nx)
                 (module_re_exports m {-++ module_import_only m-}))
    $$ text " "
    $$ ppHighLevelDecls nx (module_decls m)

-- | Generate a fragmentary parser for an attribute.
ppAttr :: Attribute -> Int -> Doc
ppAttr a n = (text "a"<>text (show n)) <+> text "<- getAttribute \""
                                       <> ppXName (attr_name a)
                                       <> text "\" e pos"
-- | Generate a fragmentary parser for an element.
ppElem :: NameConverter -> Element -> Doc
ppElem nx e@Element{}
    | elem_byRef e    = ppElemModifier (elem_modifier e)
                                       (text "element"
                                        <> ppUnqConId nx (elem_name e))
    | otherwise       = ppElemModifier (elem_modifier e)
                                       (text "parseSchemaType \""
                                        <> ppXName (elem_name e)
                                        <> text "\"")
ppElem nx e@AnyElem{} = ppElemModifier (elem_modifier e)
                          (text "parseAnyElement")
ppElem nx e@Text{}    = text "parseText"
ppElem nx e@OneOf{}   = ppElemModifier (elem_modifier e)
                          (text "oneOf" <+> ppvList "[" "," "]"
                                                    (ppOneOf n)
                                                    (zip (elem_oneOf e) [1..n]))
  where
    n = length (elem_oneOf e)
    ppOneOf n (e,i) = text "fmap" <+> text (ordinal i ++"Of"++show n)
                      <+> parens (ppSeqElem e)
    ordinal i | i <= 20   = ordinals!!i
              | otherwise = "Choice" ++ show i
    ordinals = ["Zero","One","Two","Three","Four","Five","Six","Seven","Eight"
               ,"Nine","Ten","Eleven","Twelve","Thirteen","Fourteen","Fifteen"
               ,"Sixteen","Seventeen","Eighteen","Nineteen","Twenty"]
    ppSeqElem []  = PP.empty
    ppSeqElem [e] = ppElem nx e
    ppSeqElem es  = text ("return ("++replicate (length es-1) ','++")")
                    <+> vcat (map (\e-> text "`apply`" <+> ppElem nx e) es)

-- | Convert multiple HaskellTypeModel Decls to Haskell source text.
ppHighLevelDecls :: NameConverter -> [Decl] -> Doc
ppHighLevelDecls nx hs = vcat (intersperse (text " ")
                                           (map (ppHighLevelDecl nx) hs))

-- | Convert a single Haskell Decl into Haskell source text.
ppHighLevelDecl :: NameConverter -> Decl -> Doc

ppHighLevelDecl nx (NamedSimpleType t s comm) =
    ppComment Before comm
    $$ text "type" <+> ppUnqConId nx t <+> text "=" <+> ppConId nx s
    $$ text "-- No instances required: synonym is isomorphic to the original."

ppHighLevelDecl nx (RestrictSimpleType t s r comm) =
    ppComment Before comm
    $$ text "newtype" <+> ppUnqConId nx t <+> text "="
                      <+> ppUnqConId nx t <+> ppConId nx s
    $$ text "instance Eq" <+> ppUnqConId nx t
    $$ text "instance Show" <+> ppUnqConId nx t
    $$ text "instance Restricts" <+> ppUnqConId nx t <+> ppConId nx s
    $$ text "instance SchemaType" <+> ppUnqConId nx t
    $$ text "instance SimpleType" <+> ppUnqConId nx t

ppHighLevelDecl nx (ExtendSimpleType t s as comm) =
    ppComment Before comm
    $$ text "data" <+> ppUnqConId nx t
    $$ text "data" <+> ppConId nx t_attrs
    $$ text "instance Eq" <+> ppUnqConId nx t
    $$ text "instance Eq" <+> ppConId nx t_attrs
    $$ text "instance Show" <+> ppUnqConId nx t
    $$ text "instance Show" <+> ppConId nx t_attrs
    $$ text "instance SchemaType" <+> ppUnqConId nx t
    $$ text "instance Extension"  <+> ppUnqConId nx t <+> ppConId nx s
  where
    t_attrs = let (XName (N t_base)) = t in XName (N (t_base++"Attributes"))

ppHighLevelDecl nx (UnionSimpleTypes t sts comm) =
    ppComment Before comm
    $$ text "data" <+> ppUnqConId nx t <+> text "=" <+> ppUnqConId nx t
    $$ text "-- Placeholder for a Union type, not yet implemented."

ppHighLevelDecl nx (EnumSimpleType t [] comm) =
    ppComment Before comm
    $$ text "data" <+> ppUnqConId nx t
ppHighLevelDecl nx (EnumSimpleType t is comm) =
    ppComment Before comm
    $$ text "data" <+> ppUnqConId nx t
    $$ text "instance Eq" <+> ppUnqConId nx t
    $$ text "instance Show" <+> ppUnqConId nx t
    $$ text "instance Enum" <+> ppUnqConId nx t
    $$ text "instance SchemaType" <+> ppUnqConId nx t
    $$ text "instance SimpleType" <+> ppUnqConId nx t

ppHighLevelDecl nx (ElementsAttrs t es as comm) =
    ppComment Before comm
    $$ text "data" <+> ppUnqConId nx t
    $$ text "instance Eq" <+> ppUnqConId nx t
    $$ text "instance Show" <+> ppUnqConId nx t
    $$ text "instance SchemaType" <+> ppUnqConId nx t

ppHighLevelDecl nx (ElementsAttrsAbstract t insts comm) =
    ppComment Before comm
    $$ text "data" <+> ppUnqConId nx t
    $$ text "instance Eq" <+> ppUnqConId nx t
    $$ text "instance Show" <+> ppUnqConId nx t
    $$ text "instance SchemaType" <+> ppUnqConId nx t

ppHighLevelDecl nx (ElementOfType e@Element{}) =
    ppComment Before (elem_comment e)
    $$ (text "element" <> ppUnqConId nx (elem_name e)) <+> text "::"
        <+> text "XMLParser" <+> ppConId nx (elem_type e)
    $$ (text "elementToXML" <> ppUnqConId nx (elem_name e)) <+> text "::"
        <+> ppConId nx (elem_type e) <+> text "-> [Content ()]"


ppHighLevelDecl nx e@(ElementAbstractOfType n t substgrp comm)
    | any notInScope substgrp
                = (text "element" <> ppUnqConId nx n) <+> text "::"
                      <+> text "XMLParser" <+> ppConId nx t
                $$ (text "elementToXML" <> ppUnqConId nx n) <+> text "::"
                    <+> ppConId nx t <+> text "-> [Content ()]"
    | otherwise = ppElementAbstractOfType nx e
  where
    notInScope (_,Just _)  = True
    notInScope (_,Nothing) = False

ppHighLevelDecl nx (Choice t es comm) =
    ppComment Before comm
    $$ text "data" <+> ppUnqConId nx t
    $$ text "instance Eq" <+> ppUnqConId nx t
    $$ text "instance Show" <+> ppUnqConId nx t

-- Comment out the Group for now.  Groups get inlined into the ComplexType
-- where they are used, so it may not be sensible to declare them separately
-- as well.
ppHighLevelDecl nx (Group t es comm) = PP.empty
--  ppComment Before comm
--  $$ text "data" <+> ppConId nx t <+> text "="
--                 <+> ppConId nx t <+> hsep (map (ppConId nx . elem_type) es)

-- Possibly we want to declare a really more restrictive type, e.g. 
--    to remove optionality, (Maybe Foo) -> (Foo), [Foo] -> Foo
--    consequently the "restricts" method should do a proper translation,
--    not merely an unwrapping.
ppHighLevelDecl nx (RestrictComplexType t s comm) =
    ppComment Before comm
    $$ text "newtype" <+> ppUnqConId nx t <+> text "="
                                       <+> ppUnqConId nx t <+> ppConId nx s
    $$ text "-- plus different (more restrictive) parser"
    $$ text "instance Eq" <+> ppUnqConId nx t
    $$ text "instance Show" <+> ppUnqConId nx t
    $$ text "instance Restricts" <+> ppUnqConId nx t <+> ppConId nx s
    $$ text "instance SchemaType" <+> ppUnqConId nx t

{-
ppHighLevelDecl nx (ExtendComplexType t s es as _ comm)
    | length es + length as = 1 =
    ppComment Before comm
    $$ text "data" <+> ppConId nx t <+> text "="
                                    <+> ppConId nx t <+> ppConId nx s
                                    <+> ppFields nx t es as
    $$ text "instance Extension" <+> ppConId nx t <+> ppConId nx s
                                 <+> ppAuxConId nx t <+> text "where"
        $$ nest 4 (text "supertype (" <> ppConId nx t <> text " s e) = s"
                   $$ text "extension (" <> ppConId nx t <> text " s e) = e")
-}

ppHighLevelDecl nx (ExtendComplexType t s oes oas es as
                                      fwdReqd absSup grandsuper comm) =
    ppHighLevelDecl nx (ElementsAttrs t (oes++es) (oas++as) comm)
    $$ ppExtension nx t s fwdReqd absSup oes oas es as
    $$ (if not (null grandsuper) -- && not (isJust fwdReqd)
        then ppSuperExtension nx s grandsuper (t,Nothing)
        else empty)

ppHighLevelDecl nx (ExtendComplexTypeAbstract t s insts
                                              fwdReqd grandsuper comm) =
    ppHighLevelDecl nx (ElementsAttrsAbstract t insts comm)
    $$ ppExtension nx t s fwdReqd True [] [] [] []
--  $$ if not (null grandsuper)
--     then vcat (map (ppSuperExtension nx t grandsuper) insts)
--                     -- FIXME some instances are missing!
--     else empty

ppHighLevelDecl nx (XSDInclude m comm) =
    ppComment After comm
    $$ text "import {-# SOURCE #-}" <+> ppModId nx m

ppHighLevelDecl nx (XSDImport m ma comm) =
    ppComment After comm
    $$ text "import {-# SOURCE #-}" <+> ppModId nx m
                     <+> maybe empty (\a->text "as"<+>ppConId nx a) ma

ppHighLevelDecl nx (XSDComment comm) =
    ppComment Before comm

--------------------------------------------------------------------------------

-- | Instances that depend on FwdDecl'd types, need to be declared in a
--   different module.  So they have been separated out from ppHighLevelDecl.
ppHighLevelInstances :: NameConverter -> Decl -> Doc
ppHighLevelInstances nx (ElementsAttrsAbstract t insts comm) =
    text "instance SchemaType" <+> ppUnqConId nx t

ppHighLevelInstances nx e@(ElementAbstractOfType n t substgrp comm)
    | any notInScope substgrp = ppElementAbstractOfType nx e
    | otherwise = empty
  where
    notInScope (_,Just _)  = True
    notInScope (_,Nothing) = False

ppHighLevelInstances nx (ExtendComplexType t s oes oas es as
                                      fwdReqd absSup grandsuper comm) =
    empty
--  ppExtension nx t s fwdReqd absSup oes oas es as
--  $$ (if not (null grandsuper) && isJust fwdReqd
--      then ppSuperExtension nx s grandsuper (t,Nothing)
--      else empty)

ppHighLevelInstances nx (ExtendComplexTypeAbstract t s insts
                                                   fwdReqd grandsuper comm) =
    ppHighLevelInstances nx (ElementsAttrsAbstract t insts comm)
--  $$ ppExtension nx t s fwdReqd True [] [] [] []
--  $$ if not (null grandsuper)
--     then vcat (map (ppSuperExtension nx t grandsuper) insts)
--                     -- FIXME some instances are missing!
--     else empty

ppElementAbstractOfType nx (ElementAbstractOfType n t substgrp comm) =
    ppComment Before comm
    $$ (text "element" <> ppUnqConId nx n) <+> text "::"
        <+> text "XMLParser" <+> ppConId nx t

--------------------------------------------------------------------------------

-- | Generate an instance of the Extension class for a subtype/supertype pair.
ppExtension :: NameConverter -> XName -> XName -> Maybe XName -> Bool ->
               [Element] -> [Attribute] -> [Element] -> [Attribute] -> Doc
ppExtension nx t s fwdReqd abstractSuper oes oas es as =
    text "instance Extension" <+> ppUnqConId nx t <+> ppConId nx s

-- | Generate an instance of the Extension class for a type and its
--   "grand"-supertype, that is, the supertype of its supertype.
ppSuperExtension :: NameConverter -> XName -> [XName]
                    -> (XName,Maybe XName) -> Doc
{-
ppSuperExtension nx super (grandSuper:_) (t,Nothing) =
    text "instance Extension" <+> ppUnqConId nx t <+> ppConId nx grandSuper
                              <+> text "where"
    $$ nest 4 (text "supertype = (supertype ::"
                                           <+> ppUnqConId nx super
                                           <+> text "->"
                                           <+> ppConId nx grandSuper <> text ")"
              $$ nest 12 (text ". (supertype ::"
                                           <+> ppUnqConId nx t
                                           <+> text "->"
                                           <+> ppConId nx super <> text ")"))
-}
ppSuperExtension nx super (grandSuper:_) (t,Just mod) =  -- fwddecl
    -- FIXME: generate comment for all of the grandSupers.
    text "-- instance Extension" <+> ppUnqConId nx t <+> ppConId nx grandSuper
    $$ text "--   will be declared in module" <+> ppModId nx mod
ppSuperExtension nx super grandSupers (t,Nothing) =
    vcat (map (ppSuper t) (map reverse . drop 2 . inits $ super: grandSupers))
  where
    ppSuper :: XName -> [XName] -> Doc
    ppSuper t gss@(gs:_) =
        text "instance Extension" <+> ppUnqConId nx t <+> ppConId nx gs

-- | Generate named fields from elements and attributes.
ppFields :: NameConverter -> XName -> [Element] -> [Attribute] -> Doc
ppFields nx t es as | null es && null as = empty
ppFields nx t es as =  ppvList "{" "," "}" id fields
  where
    fields = map (ppFieldAttribute nx t) as ++
             zipWith (ppFieldElement nx t) es [0..]

-- | Generate a single named field from an element.
ppFieldElement :: NameConverter -> XName -> Element -> Int -> Doc
ppFieldElement nx t e@Element{} _ = ppFieldId nx t (elem_name e)
                                        <+> text "::" <+> ppElemTypeName nx id e
                                    $$ ppComment After (elem_comment e)
ppFieldElement nx t e@OneOf{}   i = ppFieldId nx t (XName $ N $"choice"++show i)
                                        <+> text "::" <+> ppElemTypeName nx id e
                                    $$ ppComment After (elem_comment e)
ppFieldElement nx t e@AnyElem{} i = ppFieldId nx t (XName $ N $"any"++show i)
                                        <+> text "::" <+> ppElemTypeName nx id e
                                    $$ ppComment After (elem_comment e)
ppFieldElement nx t e@Text{}    i = ppFieldId nx t (XName $ N $"text"++show i)
                                        <+> text "::" <+> ppElemTypeName nx id e

-- | What is the name of the type for an Element (or choice of Elements)?
ppElemTypeName :: NameConverter -> (Doc->Doc) -> Element -> Doc
ppElemTypeName nx brack e@Element{} =
    ppTypeModifier (elem_modifier e) brack $ ppConId nx (elem_type e)
ppElemTypeName nx brack e@OneOf{}   = 
    brack $ ppTypeModifier (elem_modifier e) parens $
    text "OneOf" <> text (show (length (elem_oneOf e)))
     <+> hsep (map ppSeq (elem_oneOf e))
  where
    ppSeq []  = text "()"
    ppSeq [e] = ppElemTypeName nx parens e
    ppSeq es  = text "(" <> hcat (intersperse (text ",")
                                     (map (ppElemTypeName nx parens) es))
                         <> text ")"
ppElemTypeName nx brack e@AnyElem{} =
    brack $ ppTypeModifier (elem_modifier e) id $
    text "AnyElement"
ppElemTypeName nx brack e@Text{} =
    text "String"

-- | Generate a single named field from an attribute.
ppFieldAttribute :: NameConverter -> XName -> Attribute -> Doc
ppFieldAttribute nx t a = ppFieldId nx t (attr_name a) <+> text "::"
                                   <+> ppConId nx (attr_type a)
                          $$ ppComment After (attr_comment a)

-- | Generate a list or maybe type name (possibly parenthesised).
ppTypeModifier :: Modifier -> (Doc->Doc) -> Doc -> Doc
ppTypeModifier Single   _ d  = d
ppTypeModifier Optional k d  = k $ text "Maybe" <+> k d
ppTypeModifier (Range (Occurs Nothing Nothing))  _ d = d
ppTypeModifier (Range (Occurs (Just 0) Nothing)) k d = k $ text "Maybe" <+> k d
ppTypeModifier (Range (Occurs _ _))              _ d = text "[" <> d <> text "]"

-- | Generate a parser for a list or Maybe value.
ppElemModifier Single    doc = doc
ppElemModifier Optional  doc = text "optional" <+> parens doc
ppElemModifier (Range (Occurs Nothing Nothing))  doc = doc
ppElemModifier (Range (Occurs (Just 0) Nothing)) doc = text "optional"
                                                       <+> parens doc
ppElemModifier (Range o) doc = text "between" <+> (parens (text (show o))
                                                  $$ parens doc)

-- | Split long lines of comment text into a paragraph with a maximum width.
paragraph :: Int -> String -> String
paragraph n s = go n (words s)
    where go i []     = []
          go i (x:xs) | len<i     =       x++" "++go (i-len-1) xs
                      | otherwise = "\n"++x++" "++go (n-len-1) xs
              where len = length x

uniqueify :: [Element] -> [Element]
uniqueify = go []
  where
    go seen [] = []
    go seen (e@Element{}:es)
        | show (elem_name e) `elem` seen
                    = let fresh = new (`elem`seen) (elem_name e) in
                      e{elem_name=fresh} : go (show fresh:seen) es
        | otherwise = e: go (show (elem_name e): seen) es
    go seen (e:es)  = e : go seen es
    new pred (XName (N n))     = XName $ N $ head $
                                 dropWhile pred [(n++show i) | i <- [2..]]
    new pred (XName (QN ns n)) = XName $ QN ns $ head $
                                 dropWhile pred [(n++show i) | i <- [2..]]

