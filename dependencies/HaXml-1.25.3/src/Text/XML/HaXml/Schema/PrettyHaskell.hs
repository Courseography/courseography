-- | Pretty-print the internal Haskell model of XSD datatypes to a real
--   Haskell module containing type declarations, and instances for parsing
--   (and printing - though not yet implemented) values of those datatypes
--   from(/to) XML.
module Text.XML.HaXml.Schema.PrettyHaskell
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
import Data.Maybe (isJust,fromJust,fromMaybe,catMaybes)
import Data.Char (toLower)

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

-- | Generate aligned haddock-style docs for choices (where each choice
--   has its own documentation, but haddock cannot place it directly next
--   to the appropriate component.
ppCommentForChoice :: CommentPosition -> Comment -> [[Element]] -> Doc
ppCommentForChoice pos outer nested =
    text "--" <+> text (case pos of Before -> "|"; After -> "^") <+> text c
    $$ vcat (map (\x-> text "--  " <+> text x) cs)
    $$ vcat (map (\x-> text "--  " <+> text x) bullets)
  where
    (c:cs)  = lines intro
    intro   = maybe "Choice between:"
                    (\s-> paragraph 60 s++"\n\nChoice between:")
                    outer
    bullets = concatMap lines
              $ zipWith (\n seq-> case seq of
                              [x]-> "\n("++show n++") "++paragraph 56 x
                              _  -> "\n("++show n++") Sequence of:"
                                    ++ concatMap (\s->"\n\n  * "
                                                      ++paragraph 52 s)
                                                 seq)
                        [1..]
              $ map (map safeComment)
              $ nested
    safeComment Text = "mixed text"
    safeComment e@Element{} = fromMaybe (xname $ elem_name e) (elem_comment e)
    safeComment e@_         = fromMaybe ("unknown") (elem_comment e)
    xname (XName (N x))     = x
    xname (XName (QN ns x)) = nsPrefix ns++":"++x

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
    $$ text "import Text.XML.HaXml.OneOfN"
    $$ (case module_xsd_ns m of
         Nothing -> text "import Text.XML.HaXml.Schema.PrimitiveTypes as Xsd"
         Just ns -> text "import qualified Text.XML.HaXml.Schema.PrimitiveTypes as"<+>ppConId nx ns)
    $$ vcat (map (ppHighLevelDecl nx)
                 (module_re_exports m ++ module_import_only m))
    $$ text " "
    $$ text "-- Some hs-boot imports are required, for fwd-declaring types."
    $$ vcat (map ppFwdDecl $ concatMap imports $ module_decls m)
    $$ vcat (map ppFwdElem $ concatMap importElems $ module_decls m)
    $$ text " "
    $$ ppHighLevelDecls nx (module_decls m)

  where
    imports (ElementsAttrsAbstract _ deps _) = deps
    imports (ExtendComplexTypeAbstract _ _ deps _ _ _) = deps
    imports _ = []

    importElems (ElementAbstractOfType _ _ deps _) = deps
    importElems _ = []

    ppFwdDecl (_,   Nothing)  = empty
    ppFwdDecl (name,Just mod) = text "import {-# SOURCE #-}" <+> ppModId nx mod
                                <+> text "(" <+> ppConId nx name <+> text ")"

    ppFwdElem (_,   Nothing)  = empty
    ppFwdElem (name,Just mod) = text "import {-# SOURCE #-}" <+> ppModId nx mod
                                <+> text "("
                                    <+> (text "element" <> ppUnqConId nx name)
                                    <> (text ", elementToXML" <> ppUnqConId nx name)
                                <+> text ")"


-- | Generate a fragmentary parser for an attribute.
ppAttr :: Attribute -> Int -> Doc
ppAttr a n = (text "a"<>text (show n)) <+> text "<-"
                                       <+> (if attr_required a then empty
                                                 else text "optional $")
                                       <+> text "getAttribute \""
                                       <> ppXName (attr_name a)
                                       <> text "\" e pos"

-- | Generate a fragmentary toXML for an attribute.
toXmlAttr :: Attribute -> Doc
toXmlAttr a = (if attr_required a then id
                                  else (\d-> text "maybe []" <+> parens d))
              (text "toXMLAttribute \"" <> ppXName (attr_name a) <> text "\"")

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
ppElem nx e@OneOf{}   = ppElemModifier (liftedElemModifier e)
                          (text "oneOf'" <+> ppvList "[" "," "]"
                                                    (ppOneOf n)
                                                    (zip (elem_oneOf e) [1..n]))
  where
    n = length (elem_oneOf e)
    ppOneOf n (e,i) = text "(\"" <> hsep (map (ppElemTypeName nx id)
                                         . cleanChoices $ e)
                      <> text "\","
                      <+> text "fmap" <+> text (ordinal i ++"Of"++show n)
                          <+> parens (ppSeqElem . cleanChoices $ e)
                      <> text ")"
    ordinal i | i <= 20   = ordinals!!i
              | otherwise = "Choice" ++ show i
    ordinals = ["Zero","One","Two","Three","Four","Five","Six","Seven","Eight"
               ,"Nine","Ten","Eleven","Twelve","Thirteen","Fourteen","Fifteen"
               ,"Sixteen","Seventeen","Eighteen","Nineteen","Twenty"]
    ppSeqElem []  = PP.empty
    ppSeqElem [e] = ppElem nx e
    ppSeqElem es  = text ("return ("++replicate (length es-1) ','++")")
                    <+> vcat (map (\e-> text "`apply`" <+> ppElem nx e) es)

-- | Generate a fragmentary toXML for an element.  Fragment must still be
--   applied to an actual element value.
toXmlElem :: NameConverter -> Element -> Doc
toXmlElem nx e@Element{}
    | elem_byRef e    = xmlElemModifier (elem_modifier e)
                                        (text "elementToXML"
                                        <> ppUnqConId nx (elem_name e))
    | otherwise       = xmlElemModifier (elem_modifier e)
                                        (text "schemaTypeToXML \""
                                        <> ppXName (elem_name e)
                                        <> text "\"")
toXmlElem nx e@AnyElem{} = xmlElemModifier (elem_modifier e)
                                           (text "toXMLAnyElement")
toXmlElem nx e@Text{}    = text "toXMLText"
toXmlElem nx e@OneOf{}   = xmlElemModifier (liftedElemModifier e)
                           (text "foldOneOf" <> text (show n)
                           <+> ppvList "" "" "" xmlOneOf (elem_oneOf e))
  where
    n = length (elem_oneOf e)
    xmlOneOf e = parens (xmlSeqElem . cleanChoices $ e)
    xmlSeqElem []  = PP.empty
    xmlSeqElem [e] = toXmlElem nx e
    xmlSeqElem es  = text "\\ (" <> hcat (intersperse (text ",") vars)
                     <> text ") -> concat"
                     <+> ppvList "[" "," "]" (\(e,v)-> toXmlElem nx e <+> v)
                                             (zip es vars)
        where vars = map (text.(:[])) . take (length es) $ ['a'..'z']

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
                      <+> text "deriving (Eq,Show)"
    $$ text "instance Restricts" <+> ppUnqConId nx t <+> ppConId nx s
                      <+> text "where"
        $$ nest 4 (text "restricts (" <> ppUnqConId nx t <+> text "x) = x")
    $$ text "instance SchemaType" <+> ppUnqConId nx t <+> text "where"
        $$ nest 4 (text "parseSchemaType s = do" 
                  $$ nest 4 (text "e <- element [s]"
                           $$ text "commit $ interior e $ parseSimpleType")
                  )
        $$ nest 4 (text "schemaTypeToXML s ("<> ppUnqConId nx t <+> text "x) = " 
                  $$ nest 4 (text "toXMLElement s [] [toXMLText (simpleTypeText x)]")
                  )
    $$ text "instance SimpleType" <+> ppUnqConId nx t <+> text "where"
        $$ nest 4 (text "acceptingParser = fmap" <+> ppUnqConId nx t
                                                 <+> text "acceptingParser"
                   -- XXX should enforce the restrictions somehow.  (?)
                   $$ text "-- XXX should enforce the restrictions somehow?"
                   $$ text "-- The restrictions are:"
                   $$ vcat (map ((text "--     " <+>) . ppRestrict) r))
        $$ nest 4 (text "simpleTypeText (" <> ppUnqConId nx t
                                          <+> text "x) = simpleTypeText x")
  where
    ppRestrict (RangeR occ comm)     = text "(RangeR"
                                         <+> ppOccurs occ <>  text ")"
    ppRestrict (Pattern regexp comm) = text ("(Pattern "++regexp++")")
    ppRestrict (Enumeration items)   = text "(Enumeration"
                                         <+> hsep (map (text . fst) items)
                                         <>  text ")"
    ppRestrict (StrLength occ comm)  = text "(StrLength"
                                         <+> ppOccurs occ <>  text ")"
    ppOccurs = parens . text . show

ppHighLevelDecl nx (ExtendSimpleType t s as comm) =
    ppComment Before comm
    $$ text "data" <+> ppUnqConId nx t <+> text "="
                                    <+> ppUnqConId nx t <+> ppConId nx s
                                    <+> ppConId nx t_attrs
                                    <+> text "deriving (Eq,Show)"
    $$ text "data" <+> ppConId nx t_attrs <+> text "=" <+> ppConId nx t_attrs
        $$ nest 4 (ppFields nx t_attrs [] as
                  $$ text "deriving (Eq,Show)")
    $$ text "instance SchemaType" <+> ppUnqConId nx t <+> text "where"
        $$ nest 4 (text "parseSchemaType s = do" 
                  $$ nest 4 (text "(pos,e) <- posnElement [s]"
                            $$ text "commit $ do"
                            $$ nest 2
                                  (vcat (zipWith ppAttr as [0..])
                                  $$ text "reparse [CElem e pos]"
                                  $$ text "v <- parseSchemaType s"
                                  $$ text "return $" <+> ppUnqConId nx t
                                                     <+> text "v"
                                                     <+> attrsValue as)
                            )
                  )
        $$ nest 4 (text "schemaTypeToXML s ("<> ppUnqConId nx t
                                             <+> text "bt at) ="
                  $$ nest 4 (text "addXMLAttributes"
                             <+> ppvList "[" "," "]"
                                     (\a-> toXmlAttr a <+> text "$"
                                         <+> ppFieldId nx t_attrs (attr_name a)
                                         <+> text "at")
                                     as
                             $$ nest 4 (text "$ schemaTypeToXML s bt"))
                  )
    $$ text "instance Extension" <+> ppUnqConId nx t <+> ppConId nx s
                                 <+> text "where"
        $$ nest 4 (text "supertype (" <> ppUnqConId nx t <> text " s _) = s")
  where
    t_attrs = let (XName (N t_base)) = t in XName (N (t_base++"Attributes"))

    attrsValue [] = ppConId nx t_attrs
    attrsValue as = parens (ppConId nx t_attrs <+>
                            hsep [text ("a"++show n) | n <- [0..length as-1]])

    -- do element [s]
    --    blah <- attribute foo
    --    interior e $ do
    --        simple <- parseText acceptingParser
    --        return (T simple blah)

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
        $$ nest 4 ( ppvList "=" "|" "deriving (Eq,Show,Enum)" item is )
    $$ text "instance SchemaType" <+> ppUnqConId nx t <+> text "where"
        $$ nest 4 (text "parseSchemaType s = do" 
                  $$ nest 4 (text "e <- element [s]"
                           $$ text "commit $ interior e $ parseSimpleType")
                  )
        $$ nest 4 (text "schemaTypeToXML s x = " 
                  $$ nest 4 (text "toXMLElement s [] [toXMLText (simpleTypeText x)]")
                  )
    $$ text "instance SimpleType" <+> ppUnqConId nx t <+> text "where"
        $$ nest 4 (text "acceptingParser ="
                        <+> ppvList "" "`onFail`" "" parseItem is
                   $$ vcat (map enumText is))
  where
    item (i,c) = (ppUnqConId nx t <> text "_" <> ppConId nx i)
                 $$ ppComment After c
    parseItem (i,_) = text "do literal \"" <> ppXName i <> text "\"; return"
                           <+> (ppUnqConId nx t <> text "_" <> ppConId nx i)
    enumText  (i,_) = text "simpleTypeText"
                           <+> (ppUnqConId nx t <> text "_" <> ppConId nx i)
                           <+> text "= \"" <> ppXName i <> text "\""

ppHighLevelDecl nx (ElementsAttrs t es as comm) =
    ppComment Before comm
    $$ text "data" <+> ppUnqConId nx t <+> text "=" <+> ppUnqConId nx t
        $$ nest 8 (ppFields nx t (uniqueify es) as
                  $$ text "deriving (Eq,Show)")
    $$ text "instance SchemaType" <+> ppUnqConId nx t <+> text "where"
        $$ nest 4 (text "parseSchemaType s = do" 
                  $$ nest 4 (text "(pos,e) <- posnElement [s]"
                       --   $$ text "commit $ do"
                       --   $$ nest 2
                            $$    (vcat (zipWith ppAttr as [0..])
                                  $$ text "commit $ interior e $ return"
                                      <+> returnValue as
                                      $$ nest 4 (vcat (map ppApplyElem es))
                                  )
                            )
                  )
        $$ nest 4 (text "schemaTypeToXML s x@"<> ppUnqConId nx t <> text "{} ="
                  $$ nest 4 (text "toXMLElement s"
                             <+> ppvList "[" "," "]"
                                         (\a-> toXmlAttr a <+> text "$"
                                               <+> ppFieldId nx t (attr_name a)
                                               <+> text "x")
                                         as
                             $$ nest 4 (ppvList "[" "," "]"
                                           (\ (e,i)-> toXmlElem nx e
                                                      <+> text "$"
                                                      <+> ppFieldName nx t e i
                                                      <+> text "x")
                                           (zip es [0..]))
                            )
                  )
  where
    returnValue [] = ppUnqConId nx t
    returnValue as = parens (ppUnqConId nx t <+>
                             hsep [text ("a"++show n) | n <- [0..length as-1]])
    ppApplyElem e = text "`apply`" <+> ppElem nx e

ppHighLevelDecl nx (ElementsAttrsAbstract t [] comm) =
    ppComment Before comm
    $$ text "--  (There are no subtypes defined for this abstract type.)"
    $$ text "data" <+> ppUnqConId nx t <+> text "=" <+> ppUnqConId nx t
                   <+> text "deriving (Eq,Show)"
    $$ text "instance SchemaType" <+> ppUnqConId nx t <+> text "where"
        $$ nest 4 (text "parseSchemaType s = fail" <+> errmsg)
        $$ nest 4 (text "schemaTypeToXML s _ = toXMLElement s [] []")
  where
    errmsg = text "\"Parse failed when expecting an extension type of"
             <+> ppXName t <> text ":\\n  No extension types are known.\""
ppHighLevelDecl nx (ElementsAttrsAbstract t insts comm) =
    ppComment Before comm
    $$ text "data" <+> ppUnqConId nx t
        $$ nest 8 (ppvList "=" "|" "" ppAbstrCons insts
                  $$ text "deriving (Eq,Show)")
--  $$ text "-- instance SchemaType" <+> ppUnqConId nx t
--      <+> text "(declared in Instance module)"
--  *** Declare instance here
    $$ text "instance SchemaType" <+> ppUnqConId nx t <+> text "where"
        $$ nest 4 (text "parseSchemaType s = do" 
                  $$ nest 4 (vcat (intersperse (text "`onFail`")
                                               (map ppParse insts)
                                   ++ [text "`onFail` fail" <+> errmsg])))
        $$ nest 4 (vcat (map toXML insts))
--  $$ text ""
--  $$ vcat (map ppFwdDecl $ filter (isJust . snd) insts)
  where
    ppAbstrCons (name,Nothing)  = con name <+> ppConId nx name
    ppAbstrCons (name,Just mod) = con name <+> ppConId nx name
--  *** Declare FwdDecl type here (proxy for type declared in later module)
--  ppAbstrCons (name,Just mod) = text "forall q . (FwdDecl" <+>
--                                fwd name <+> text "q," <+>
--                                text "SchemaType q) =>" <+>
--                                con name <+>
--                                text "("<>fwd name<>text"->q)" <+> fwd name
    ppParse (name,Nothing) = text "(fmap" <+> con name <+>
                             text "$ parseSchemaType s)"
    ppParse (name,Just _)  = ppParse (name,Nothing)
--  ppParse (name,Just _)  = text "(return" <+> con name <+>
--                           text "`apply` (fmap const $ parseSchemaType s)" <+>
--                           text "`apply` return" <+> fwd name <> text ")"
--  ppFwdDecl (name,Just mod)
--         = text "-- | Proxy:" <+> ppConId nx name
--               <+> text "declared later in" <+> ppModId nx mod
--           $$ text "data" <+> fwd name <+> text "=" <+> fwd name
    errmsg = text "\"Parse failed when expecting an extension type of"
             <+> ppXName t <> text ",\\n\\\n\\  namely one of:\\n\\\n\\"
             <> hcat (intersperse (text ",")
                                  (map (ppXName . fst) insts))
             <> text "\""
--  fwd name = ppFwdConId nx name
    con name = ppJoinConId nx t name
    -- This is probably an unportable hack, but because an abstract type never
    -- has an element in its own name, we need to guess at the name of the
    -- possible subtype elements that could substitute for it.
    toXML (name,_) = text "schemaTypeToXML _s ("
                     <> con name <+> text "x) = schemaTypeToXML \""
                     <> ppXName (initLower name) <> text "\" x"
    initLower (XName (N (c:cs))) = XName $ N (toLower c:cs)
    initLower (XName (QN ns (c:cs))) = XName $ QN ns (toLower c:cs)

ppHighLevelDecl nx (ElementOfType e@Element{}) =
    ppComment Before (elem_comment e)
    $$ (text "element" <> ppUnqConId nx (elem_name e)) <+> text "::"
        <+> text "XMLParser" <+> ppConId nx (elem_type e)
    $$ (text "element" <> ppUnqConId nx (elem_name e)) <+> text "="
        <+> (text "parseSchemaType \"" <> ppXName (elem_name e)  <> text "\"")
    $$ (text "elementToXML" <> ppUnqConId nx (elem_name e)) <+> text "::"
        <+> ppConId nx (elem_type e) <+> text "-> [Content ()]"
    $$ (text "elementToXML" <> ppUnqConId nx (elem_name e)) <+> text "="
        <+> (text "schemaTypeToXML \"" <> ppXName (elem_name e)  <> text "\"")

ppHighLevelDecl nx e@(ElementAbstractOfType n t [] comm) =
    ppComment Before comm
    $$ text "--  (There are no elements in any substitution group for this element.)"
    $$ (text "element" <> ppUnqConId nx n) <+> text "::"
        <+> text "XMLParser" <+> ppConId nx t
    $$ (text "element" <> ppUnqConId nx n) <+> text "="
        <+> text "fail" <+> errmsg
    $$ (text "elementToXML" <> ppUnqConId nx n) <+> text "::"
        <+> ppConId nx t <+> text "-> [Content ()]"
    $$ (text "elementToXML" <> ppUnqConId nx n) <+> text "="
        <+> (text "schemaTypeToXML \"" <> ppXName n <> text "\"")
  where
    errmsg = text "\"Parse failed when expecting an element in the substitution group for\\n\\\n\\    <"
             <> ppXName n <> text ">,\\n\\\n\\  There are no substitutable elements.\""
ppHighLevelDecl nx e@(ElementAbstractOfType n t substgrp comm)
--  | any notInScope substgrp
--              = (text "-- element" <> ppUnqConId nx n) <+> text "::"
--                    <+> text "XMLParser" <+> ppConId nx t
--              $$ text "--     declared in Instances module"
    | otherwise = ppComment Before comm
                $$ (text "element" <> ppUnqConId nx n) <+> text "::"
                    <+> text "XMLParser" <+> ppConId nx t
                $$ (text "element" <> ppUnqConId nx n) <+> text "="
                   <+> vcat (intersperse (text "`onFail`") (map ppOne substgrp)
                             ++ [text "`onFail` fail" <+> errmsg])
                $$ (text "elementToXML" <> ppUnqConId nx n) <+> text "::"
                    <+> ppConId nx t <+> text "-> [Content ()]"
                $$ (text "elementToXML" <> ppUnqConId nx n) <+> text "="
                    <+> (text "schemaTypeToXML \"" <> ppXName n <> text "\"")
            --  $$ vcat (map elementToXML substgrp)
--  | otherwise = ppElementAbstractOfType nx e
  where
    notInScope (_,Just _)  = True
    notInScope (_,Nothing) = False
    ppOne (c,Nothing) = text "fmap" <+> text "supertype" -- ppJoinConId nx t c
                        <+> (text "element" <> ppConId nx c)
    ppOne (c,Just _)  = text "fmap" <+> text "supertype" -- ppJoinConId nx t c
                        <+> (text "element" <> ppConId nx c)
                        <+> text "-- FIXME: element is forward-declared"
    errmsg = text "\"Parse failed when expecting an element in the substitution group for\\n\\\n\\    <"
             <> ppXName n <> text ">,\\n\\\n\\  namely one of:\\n\\\n\\<"
             <> hcat (intersperse (text ">, <")
                                  (map (ppXName . fst) substgrp))
             <> text ">\""
--  elementToXML (c,_) = (text "elementToXML" <> ppUnqConId nx n)
--                       <+> text "(" <> ppJoinConId nx t c
--                       <+> text " x) = elementToXML" <> ppUnqConId nx c
--                       <+> text "x"


ppHighLevelDecl nx (Choice t es comm) =
    ppComment Before comm
    $$ text "data" <+> ppUnqConId nx t
        <+> nest 4 ( ppvList "=" "|" "" choices (zip es [1..])
                   $$ text "deriving (Eq,Show)" )
  where
    choices (e,n) = (ppUnqConId nx t <> text (show n))
                    <+> ppConId nx (elem_type e)

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
                                       <+> text "deriving (Eq,Show)"
    $$ text "-- plus different (more restrictive) parser"
    $$ text "-- (parsing restrictions currently unimplemented)"
    $$ text "instance Restricts" <+> ppUnqConId nx t <+> ppConId nx s
                                 <+> text "where"
        $$ nest 4 (text "restricts (" <> ppUnqConId nx t <+> text "x) = x")
    $$ text "instance SchemaType" <+> ppUnqConId nx t <+> text "where"
        $$ nest 4 (text "parseSchemaType = fmap " <+> ppUnqConId nx t <+>
                   text ". parseSchemaType")
		-- XXX should enforce the restriction.
        $$ nest 4 (text "schemaTypeToXML s (" <> ppUnqConId nx t <+> text "x)")
                   <+> text "= schemaTypeToXML s x"

{-
ppHighLevelDecl nx (ExtendComplexType t s es as _ comm)
    | length es + length as = 1 =
    ppComment Before comm
    $$ text "data" <+> ppConId nx t <+> text "="
                                    <+> ppConId nx t <+> ppConId nx s
                                    <+> ppFields nx t es as
                                    <+> text "deriving (Eq,Show)"
    $$ text "instance Extension" <+> ppConId nx t <+> ppConId nx s
                                 <+> ppAuxConId nx t <+> text "where"
        $$ nest 4 (text "supertype (" <> ppConId nx t <> text " s e) = s"
                   $$ text "extension (" <> ppConId nx t <> text " s e) = e")
-}

ppHighLevelDecl nx (ExtendComplexType t s oes oas es as
                                      fwdReqd absSup grandsuper comm) =
    ppHighLevelDecl nx (ElementsAttrs t (oes++es) (oas++as) comm)
    $$ ppExtension nx t s fwdReqd absSup oes oas es as
    $$ (if not (null grandsuper) -- && not (isJust fwdReqd) -- && isJust fwdReqd
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
    $$ text "import" <+> ppModId nx m

ppHighLevelDecl nx (XSDImport m ma comm) =
    ppComment After comm
    $$ text "import" <+> ppModId nx m
                     <+> maybe empty (\a->text "as"<+>ppConId nx a) ma

ppHighLevelDecl nx (XSDComment comm) =
    ppComment Before comm

{-------------------------------------------------------------------------------

-- | Instances that depend on FwdDecl'd types, need to be declared in a
--   different module.  So they have been separated out from ppHighLevelDecl.
ppHighLevelInstances :: NameConverter -> Decl -> Doc
ppHighLevelInstances nx (ElementsAttrsAbstract t insts comm) =
    text "instance SchemaType" <+> ppUnqConId nx t <+> text "where"
        $$ nest 4 (text "parseSchemaType s = do" 
                  $$ nest 4 (vcat (intersperse (text "`onFail`")
                                               (map ppParse insts)
                                   ++ [text "`onFail` fail" <+> errmsg])))
  where
    ppParse (name,Nothing) = text "(fmap" <+> con name <+>
                             text "$ parseSchemaType s)"
    ppParse (name,Just _)  = text "(return" <+> con name <+>
                             text "`apply` (fmap const $ parseSchemaType s)" <+>
                             text "`apply` return" <+> fwd name <> text ")"
    errmsg = text "\"Parse failed when expecting an extension type of"
             <+> ppXName t <> text ",\\n\\\n\\  namely one of:\\n\\\n\\"
             <> hcat (intersperse (text ",")
                                  (map (ppXName . fst) insts))
             <> text "\""
    fwd name = ppFwdConId nx name
    con name = ppJoinConId nx t name

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
    $$ (text "element" <> ppUnqConId nx n) <+> text "="
       <+> vcat (intersperse (text "`onFail`") (map ppOne substgrp)
                 ++ [text "`onFail` fail" <+> errmsg])
  where
    ppOne (c,Nothing) = text "fmap" <+> text "supertype" -- ppJoinConId nx t c
                        <+> (text "element" <> ppConId nx c)
    ppOne (c,Just _)  = text "fmap" <+> text "supertype" -- ppJoinConId nx t c
                        <+> (text "element" <> ppConId nx c)
                        <+> text "-- FIXME: element is forward-declared"
    errmsg = text "\"Parse failed when expecting an element in the substitution group for\\n\\\n\\    <"
             <> ppXName n <> text ">,\\n\\\n\\  namely one of:\\n\\\n\\<"
             <> hcat (intersperse (text ">, <")
                                  (map (ppXName . fst) substgrp))
             <> text ">\""

----------------------------------------------------------------------------- -}

--------------------------------------------------------------------------------

-- | Generate an instance of the Extension class for a subtype/supertype pair.
ppExtension :: NameConverter -> XName -> XName -> Maybe XName -> Bool ->
               [Element] -> [Attribute] -> [Element] -> [Attribute] -> Doc
ppExtension nx t s fwdReqd abstractSuper oes oas es as =
    text "instance Extension" <+> ppUnqConId nx t <+> ppConId nx s
                              <+> text "where"
       $$ (if abstractSuper then
           nest 4 (text "supertype v" <+> text "="
                                      <+> ppJoinConId nx s t <+>
                                 --   (if isJust fwdReqd
                                 --    then text "(\\_-> v)" <+> ppFwdConId nx t
                                 --    else text "v")
                                      text "v")
           else
           nest 4 (text "supertype (" <> ppType t (oes++es) (oas++as)
                                      <> text ") ="
                                      $$ nest 11 (ppType s oes oas) ))
--  $$ (if isJust fwdReqd then
--     -- text "data" <+> fwd t <+> text "=" <+> fwd t $$  -- already defined
--        text ""
--        $$ text "-- | Proxy" <+> fwd t <+> text "was declared earlier in"
--                   <+> ppModId nx (fromJust fwdReqd)
--        $$ text "instance FwdDecl" <+> fwd t <+> ppConId nx t
--      else empty)
  where
    fwd name = ppFwdConId nx name
    ppType t es as = ppUnqConId nx t
                     <+> hsep (take (length as) [text ('a':show n) | n<-[0..]])
                     <+> hsep (take (length es) [text ('e':show n) | n<-[0..]])

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
ppSuperExtension nx super grandSupers (t,Just mod) =  -- fwddecl
    text "-- Note that" <+> ppUnqConId nx t
    <+> text "will be declared later in module" <+> ppModId nx mod
    $$ ppSuperExtension nx super grandSupers (t,Nothing)
ppSuperExtension nx super grandSupers (t,Nothing) =
    vcat (map (ppSuper t) (map reverse . drop 2 . inits $ super: grandSupers))
  where
    ppSuper :: XName -> [XName] -> Doc
    ppSuper t gss@(gs:_) =
        text "instance Extension" <+> ppUnqConId nx t <+> ppConId nx gs
                                  <+> text "where"
        $$ nest 4 (text "supertype" <+>
                      (ppvList "=" "." "" coerce (zip (tail gss++[t]) gss)))
    coerce (a,b) = text "(supertype ::" <+> ppUnqConId nx a
                                        <+> text "->"
                                        <+> ppConId nx b <> text ")"

-- | Generate named fields from elements and attributes.
ppFields :: NameConverter -> XName -> [Element] -> [Attribute] -> Doc
ppFields nx t es as | null es && null as = empty
ppFields nx t es as =  ppvList "{" "," "}" id fields
  where
    fields = map (ppFieldAttribute nx t) as ++
             zipWith (ppFieldElement nx t) es [0..]

-- | Generate a single named field (including type sig) from an element.
ppFieldElement :: NameConverter -> XName -> Element -> Int -> Doc
ppFieldElement nx t e@Element{} i = ppFieldName nx t e i
                                    <+> text "::" <+> ppElemTypeName nx id e
                                    $$ ppComment After (elem_comment e)
ppFieldElement nx t e@OneOf{}   i = ppFieldName nx t e i
                                    <+> text "::" <+> ppElemTypeName nx id e
                                    $$ ppCommentForChoice After (elem_comment e)
                                                                (elem_oneOf e)
ppFieldElement nx t e@AnyElem{} i = ppFieldName nx t e i
                                    <+> text "::" <+> ppElemTypeName nx id e
                                    $$ ppComment After (elem_comment e)
ppFieldElement nx t e@Text{}    i = ppFieldName nx t e i
                                    <+> text "::" <+> ppElemTypeName nx id e

-- | Generate a single named field (no type sig) from an element.
ppFieldName :: NameConverter -> XName -> Element -> Int -> Doc
ppFieldName nx t e@Element{} _ = ppFieldId nx t (elem_name e)
ppFieldName nx t e@OneOf{}   i = ppFieldId nx t (XName $ N $"choice"++show i)
ppFieldName nx t e@AnyElem{} i = ppFieldId nx t (XName $ N $"any"++show i)
ppFieldName nx t e@Text{}    i = ppFieldId nx t (XName $ N $"text"++show i)

-- | What is the name of the type for an Element (or choice of Elements)?
ppElemTypeName :: NameConverter -> (Doc->Doc) -> Element -> Doc
ppElemTypeName nx brack e@Element{} =
    ppTypeModifier (elem_modifier e) brack $ ppConId nx (elem_type e)
ppElemTypeName nx brack e@OneOf{}   = 
    brack $ ppTypeModifier (liftedElemModifier e) parens $
    text "OneOf" <> text (show (length (elem_oneOf e)))
     <+> hsep (map (ppSeq . cleanChoices) (elem_oneOf e))
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
                                   <+> (if attr_required a then empty
                                           else text "Maybe")
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
ppElemModifier :: Modifier -> Doc -> Doc
ppElemModifier Single    doc = doc
ppElemModifier Optional  doc = text "optional" <+> parens doc
ppElemModifier (Range (Occurs Nothing Nothing))  doc = doc
ppElemModifier (Range (Occurs (Just 0) Nothing)) doc = text "optional"
                                                       <+> parens doc
ppElemModifier (Range (Occurs (Just 0) (Just n))) doc
               | n==maxBound = text "many" <+> parens doc
ppElemModifier (Range (Occurs Nothing  (Just n))) doc
               | n==maxBound = text "many1" <+> parens doc
ppElemModifier (Range (Occurs (Just 1) (Just n))) doc
               | n==maxBound = text "many1" <+> parens doc
ppElemModifier (Range o) doc = text "between" <+> (parens (text (show o))
                                                  $$ parens doc)

-- | Generate a toXML for a list or Maybe value.
xmlElemModifier :: Modifier -> Doc -> Doc
xmlElemModifier Single    doc = doc
xmlElemModifier Optional  doc = text "maybe []" <+> parens doc
xmlElemModifier (Range (Occurs Nothing Nothing))  doc = doc
xmlElemModifier (Range (Occurs (Just 0) Nothing)) doc = text "maybe []"
                                                        <+> parens doc
xmlElemModifier (Range (Occurs _ _)) doc = text "concatMap" <+> parens doc

-- | Eliminate a Maybe type modifier, when it occurs directly inside a
--   choice construct (since a parsed Nothing would always be preferred over
--   a real value later in the choice).  Likewise, empty lists must
--   be disallowed inside choice.
cleanChoices :: [Element] -> [Element]
cleanChoices [e@Element{}] = (:[]) $
    case elem_modifier e of
      Range (Occurs (Just 0) Nothing) -> e{elem_modifier=Single}
      Range (Occurs (Just 0) max)-> e{elem_modifier=Range (Occurs (Just 1) max)}
      _ -> e
cleanChoices es = es

-- | Sometimes, a choice without a type modifier contains element sequences,
--   all of which have the same modifier. In that case, it makes sense to lift
--   the modifier (typically Maybe) to the outer layer.
liftedElemModifier :: Element -> Modifier
liftedElemModifier e@OneOf{} =
    case elem_modifier e of
      Range (Occurs Nothing Nothing) -> newModifier
      Single -> newModifier
      m -> m
  where
    newModifier = if all (\x-> case x of
                                 Text -> True
                                 _ -> case elem_modifier x of
                                        Range (Occurs (Just 0) _) -> True
                                        Optional                  -> True
                                        _                         -> False)
                         (concat (elem_oneOf e))
                  then Optional
                  else Single

-- | Split long lines of comment text into a paragraph with a maximum width.
paragraph :: Int -> String -> String
paragraph n s = go n (words s)
    where go i []     = []
          go i [x]    | len<i     =       x
                      | otherwise = "\n"++x
              where len = length x
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

