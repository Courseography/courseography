module Text.XML.HaXml.DtdToHaskell.Instance
  ( mkInstance
  ) where

import Data.List (intersperse)

import Text.XML.HaXml.DtdToHaskell.TypeDef
import Text.PrettyPrint.HughesPJ

-- | Convert typedef to appropriate instance declaration, either @XmlContent@,
--   @XmlAttributes@, or @XmlAttrType@.
mkInstance :: TypeDef -> Doc

-- no constructors - represents an element with empty content but attributes.
mkInstance (DataDef _ n fs []) =
    let (_, frattr, topat, toattr) = attrpats fs
        frretval = if null fs then ppHName n else frattr
        topatval = if null fs then ppHName n else topat
    in
    text "instance HTypeable" <+> ppHName n <+> text "where" $$
    nest 4 ( text "toHType x = Defined \"" <> ppXName n <> text "\" [] []" )
    $$
    text "instance XmlContent" <+> ppHName n <+> text "where" $$
    nest 4 (
             text "toContents" <+> topatval <+> text "=" $$
             nest 4 (text "[CElem (Elem (N \"" <> ppXName n <> text "\")"
                          <+> toattr <+> text "[]) ()]")
           $$
             text "parseContents = do" $$
             nest 4 (text "{ (Elem _ as []) <- element [\""
                             <> ppXName n <> text "\"]" $$
                     text "; return" <+> frretval $$
                     text "} `adjustErr` (\"in <" <> ppXName n
                                                  <> text ">, \"++)"
                    )
           )
    $$
    mkInstanceAttrs Same n fs

-- single constructor, "real" (non-auxiliary) type
mkInstance (DataDef False n fs [(n0,sts)]) =
    let vs = nameSupply sts
        (frpat, frattr, topat, toattr) = attrpats fs
    in
    text "instance HTypeable" <+> ppHName n <+> text "where" $$
    nest 4 ( text "toHType x = Defined \"" <> ppXName n <> text "\" [] []" )
    $$
    text "instance XmlContent" <+> ppHName n <+> text "where" $$
    nest 4 (
             text "toContents" <+> parens (mkCpat n0 topat vs) <+> text "=" $$
             nest 4 (text "[CElem (Elem (N \"" <> ppXName n <> text "\")"
                          <+> toattr <+> parens (mkToElem sts vs)
                          <> text ") ()]")
           $$
             text "parseContents = do" $$
             nest 4 (text "{ e@(Elem _"<+> frpat <+> text "_) <- element [\""
                             <> ppXName n <> text "\"]"
                     $$ text "; interior e $"
                           <+> (mkParseConstr frattr (n0,sts))
                     $$ text "} `adjustErr` (\"in <" <> ppXName n
                                                     <> text ">, \"++)")
           )
    $$
    mkInstanceAttrs Extended n fs

-- single constructor, auxiliary type (i.e. no corresponding element tag)
--   cannot be attributes here?
mkInstance (DataDef True n [] [(n0,sts)]) =
    let vs = nameSupply sts
    in
    text "instance HTypeable" <+> ppHName n <+> text "where" $$
    nest 4 ( text "toHType x = Defined \"" <> ppXName n <> text "\" [] []" )
    $$
    text "instance XmlContent" <+> ppHName n <+> text "where" $$
    nest 4 ( text "toContents" <+> parens (mkCpat n0 empty vs)
                               <+> text "="
                               $$  nest 4 (parens (mkToElem sts vs))
           $$
             text "parseContents =" <+> mkParseConstr empty (n0,sts)
           )

-- multiple constructors (real)
mkInstance (DataDef False n fs cs) =
    let _ = nameSupply cs
        (frpat, frattr, topat, toattr) = attrpats fs
        _ = if null fs then False else True
    in
    text "instance HTypeable" <+> ppHName n <+> text "where" $$
    nest 4 ( text "toHType x = Defined \"" <> ppXName n <> text "\" [] []" )
    $$
    text "instance XmlContent" <+> ppHName n <+> text "where" $$
    nest 4 ( vcat (map (mkToMult n topat toattr) cs)
           $$ text "parseContents = do "
           $$ nest 4 (text "{ e@(Elem _"<+> frpat <+> text "_) <- element [\""
                                                  <> ppXName n <> text "\"]"
                     $$ text "; interior e $ oneOf"
                     $$ nest 4 ( text "[" <+> mkParseConstr frattr (head cs)
                               $$ vcat (map (\c-> text "," <+> mkParseConstr frattr c)
                                            (tail cs))
                               $$ text "] `adjustErr` (\"in <" <> ppXName n
                                                             <> text ">, \"++)"
                               )
                     $$ text "}"
                     )
           )
    $$
    mkInstanceAttrs Extended n fs

-- multiple constructors (auxiliary)
mkInstance (DataDef True n fs cs) =
    let _ = nameSupply cs
        (_, frattr, _, _) = attrpats fs
        mixattrs = if null fs then False else True
    in
    text "instance HTypeable" <+> ppHName n <+> text "where" $$
    nest 4 ( text "toHType x = Defined \"" <> ppXName n <> text "\" [] []" )
    $$
    text "instance XmlContent" <+> ppHName n <+> text "where" $$
    nest 4 ( vcat (map (mkToAux mixattrs) cs)
           $$ text "parseContents = oneOf"
           $$ nest 4 ( text "[" <+> mkParseConstr frattr (head cs)
                     $$ vcat (map (\c-> text "," <+> mkParseConstr frattr c)
                                  (tail cs))
                     $$ text "] `adjustErr` (\"in <" <> ppXName n
                                                     <> text ">, \"++)"
                     )
           )
    $$
    mkInstanceAttrs Extended n fs

-- enumeration of attribute values
mkInstance (EnumDef n es) =
    text "instance XmlAttrType" <+> ppHName n <+> text "where" $$
    nest 4 ( text "fromAttrToTyp n (N n',v)" $$
             nest 4 (text "| n==n'     = translate (attr2str v)" $$
                     text "| otherwise = Nothing") $$
             nest 2 (text "where" <+> mkTranslate es)
           $$
             vcat (map mkToAttr es)
           )


data SameName = Same | Extended

mkInstanceAttrs        :: SameName -> Name -> AttrFields -> Doc
mkInstanceAttrs _ _ []  = empty
mkInstanceAttrs s n fs  =
    let ppName = case s of { Same-> ppHName;  Extended-> ppAName; }
    in
    text "instance XmlAttributes" <+> ppName n <+> text "where" $$
    nest 4 ( text "fromAttrs as =" $$
             nest 4 ( ppName n $$
                      nest 2 (vcat ((text "{" <+> mkFrFld n (head fs)):
                                     map (\x-> comma <+> mkFrFld n x) (tail fs)) $$
                              text "}"))
           $$
             text "toAttrs v = catMaybes " $$
             nest 4 (vcat ((text "[" <+> mkToFld (head fs)):
                           map (\x-> comma <+> mkToFld x) (tail fs)) $$
                     text "]")
           )


--                  respectively (frpat,frattr,topat,toattr)
attrpats :: AttrFields -> (Doc,Doc,Doc,Doc)
attrpats fs =
  if null fs then (text "[]", empty, empty, text "[]")
  else (text "as", parens (text "fromAttrs as"), text "as", parens (text "toAttrs as"))




-- mkFrElem :: Name -> [StructType] -> [Doc] -> Doc -> Doc
-- mkFrElem n sts vs inner =
--     foldr (frElem n) inner (zip3 sts vs cvs)
--   where
--     cvs = let ns = nameSupply2 vs
--           in zip ns (text "c0": init ns)
--     frElem _ (st,v,(cvi,cvo)) inner =
--         parens (text "\\" <> parens (v<>comma<>cvi) <> text "->" $$
--                 nest 2 inner) $$
--         parens (
--           case st of
--             (Maybe String)  -> text "fromText" <+> cvo
--             (Maybe _)       -> text "fromElem" <+> cvo
--             (List String)   -> text "many fromText" <+> cvo
--             (List _)        -> text "many fromElem" <+> cvo
--             (List1 s)       -> text "definite fromElem"
--                                <+> text "\"" <> text (show s)<> text "+\""
--                                <+> text "\"" <> ppXName n <> text "\""
--                                <+> cvo
--             (Tuple ss)  -> text "definite fromElem"
--                            <+> text "\"(" <> hcat (intersperse (text ",")
--                                                            (map (text.show) ss))
--                                           <> text ")\""
--                            <+> text "\"" <> ppXName n <> text "\""
--                            <+> cvo
--             (OneOf _)  -> text "definite fromElem"
--                            <+> text "\"OneOf\""
--                            <+> text "\"" <> ppXName n <> text "\""
--                            <+> cvo
--             (String)    -> text "definite fromText" <+> text "\"text\" \"" <>
--                                                  ppXName n <> text "\"" <+> cvo
--             (Any)       -> text "definite fromElem" <+> text "\"ANY\" \"" <>
--                                                  ppXName n <> text "\"" <+> cvo
--             (Defined m) -> text "definite fromElem" <+>
--                                  text "\"<" <> ppXName m <> text ">\" \"" <>
--                                                  ppXName m <> text "\"" <+> cvo
--             (Defaultable _ _)  -> text "nyi_fromElem_Defaultable" <+> cvo
--           )

--
{-
mkParseContents :: Name -> [StructType] -> [Doc] -> Doc -> Doc
mkParseContents n sts vs inner =
    foldr (frElem n) inner (zip3 sts vs cvs)
  where
    cvs = let ns = nameSupply2 vs
          in zip ns (text "c0": init ns)
    frElem n (st,v,(cvi,cvo)) inner =
        parens (text "\\" <> parens (v<>comma<>cvi) <> text "->" $$
                nest 2 inner) $$
        parens (
          )
-}
mkParseConstr :: Doc -> (Name, [StructType]) -> Doc
mkParseConstr frattr (c,sts) =
        fsep (text "return" <+> parens (ppHName c <+> frattr)
             : map mkParseContents sts)

mkParseContents :: StructType -> Doc
mkParseContents st =
  let ap = text "`apply`" in
          case st of
            (Maybe String)    -> ap <+> text "optional text"
            (Maybe _)         -> ap <+> text "optional parseContents"
            (List String)     -> ap <+> text "many text"
            (List _)          -> ap <+> text "many parseContents"
            (List1 _)         -> ap <+> text "parseContents"
            (Tuple _)         -> ap <+> text "parseContents"
            (OneOf _)         -> ap <+> text "parseContents"
            (StringMixed)     -> ap <+> text "text"
            (String)          -> ap <+> text "(text `onFail` return \"\")"
            (Any)             -> ap <+> text "parseContents"
            (Defined _)       -> ap <+> text "parseContents"
            (Defaultable _ _) -> ap <+> text "nyi_fromElem_Defaultable"

--
mkToElem :: [StructType] -> [Doc] -> Doc
mkToElem []  [] = text "[]"
mkToElem sts vs =
    fsep (intersperse (text "++") (zipWith toElem sts vs))
  where
    toElem st v =
      case st of
        (Maybe String)    -> text "maybe [] toText" <+> v
        (Maybe _)         -> text "maybe [] toContents" <+> v
        (List String)     -> text "concatMap toText" <+> v
        (List _)          -> text "concatMap toContents" <+> v
        (List1 _)         -> text "toContents" <+> v
        (Tuple _)         -> text "toContents" <+> v
        (OneOf _)         -> text "toContents" <+> v
        (StringMixed)     -> text "toText" <+> v
        (String)          -> text "toText" <+> v
        (Any)             -> text "toContents" <+> v
        (Defined _)       -> text "toContents" <+> v
        (Defaultable _ _) -> text "nyi_toElem_Defaultable" <+> v

-- mkRpat :: [Doc] -> Doc
-- mkRpat [v] = v
-- mkRpat vs  = (parens . hcat . intersperse comma) vs

mkCpat :: Name -> Doc -> [Doc] -> Doc
mkCpat n i vs = ppHName n <+> i <+> fsep vs

nameSupply :: [b] -> [Doc]
nameSupply  ss = take (length ss) (map char ['a'..'z']
                                  ++ map text [ a:n:[] | n <- ['0'..'9']
                                                       , a <- ['a'..'z'] ])
-- nameSupply2 ss = take (length ss) [ text ('c':v:[]) | v <- ['a'..]]

mkTranslate :: [Name] -> Doc
mkTranslate es =
    vcat (map trans es) $$
    text "translate _ = Nothing"
  where
    trans n = text "translate \"" <> ppXName n <> text "\" =" <+>
              text "Just" <+> ppHName n

mkToAttr :: Name -> Doc
mkToAttr n = text "toAttrFrTyp n" <+> ppHName n <+> text "=" <+>
             text "Just (N n, str2attr" <+> doubleQuotes (ppXName n) <> text ")"

mkFrFld :: Name -> (Name,StructType) -> Doc
mkFrFld tag (n,st) =
    ppHName n <+> text "=" <+>
    ( case st of
        (Defaultable String s) -> text "defaultA fromAttrToStr" <+>
                                                 doubleQuotes (text s)
        (Defaultable _ s)      -> text "defaultA fromAttrToTyp" <+> text s
        (Maybe String)         -> text "possibleA fromAttrToStr"
        (Maybe _)              -> text "possibleA fromAttrToTyp"
        String                 -> text "definiteA fromAttrToStr" <+>
                                                 doubleQuotes (ppXName tag)
        _                      -> text "definiteA fromAttrToTyp" <+>
                                                 doubleQuotes (ppXName tag)
    ) <+> doubleQuotes (ppXName n) <+> text "as"

mkToFld :: (Name,StructType) -> Doc
mkToFld (n,st) =
    ( case st of
        (Defaultable String _) -> text "defaultToAttr toAttrFrStr"
        (Defaultable _ _)      -> text "defaultToAttr toAttrFrTyp"
        (Maybe String)         -> text "maybeToAttr toAttrFrStr"
        (Maybe _)              -> text "maybeToAttr toAttrFrTyp"
        String                 -> text "toAttrFrStr"
        _                      -> text "toAttrFrTyp"
    ) <+> doubleQuotes (ppXName n) <+> parens (ppHName n <+> text "v")

-- mkFrAux :: Bool -> Doc -> [(Name,[StructType])] -> Doc
-- mkFrAux keeprest attrs cs = foldr frAux inner cs
--   where
--     inner = text "(Nothing, c0)"
--     rest  = if keeprest then text "rest" else text "_"
--     frAux (n,sts) innr =
--         let vs  = nameSupply sts in
--         nest 4 (text "case" <+> blah sts vs <+> text "of" $$
--                 succpat sts vs <+> text "-> (Just" <+>
--                                    parens (mkCpat n attrs vs) <> text ", rest)"
--                 $$
--                 failpat sts <+> text "->" $$ nest 4 innr
--                )
--     blah [st] [_] =
--         blahblahblah st (text "c0")
--     blah sts vs =
--         let ns = nameSupply2 vs
--             cvs = zip ns (text "c0": init ns)
--             blahblah (st,v,(cvi,cvo)) innr =
--                 parens (text "\\" <> parens (v<>comma<>cvi) <> text "->" $$
--                         nest 2 innr) $$
--                 blahblahblah st cvo
--         in
--         foldr blahblah (mkRpat (vs++[last ns])) (zip3 sts vs cvs)
--     blahblahblah st cvo = parens (
--         case st of
--           (Maybe String) -> text "fromText" <+> cvo
--           (Maybe _)      -> text "fromElem" <+> cvo
--           (List String)  -> text "many fromText" <+> cvo
--           (List _)       -> text "many fromElem" <+> cvo
--           (List1 _)      -> text "fromElem" <+> cvo
--           (Tuple _)     -> text "fromElem" <+> cvo      -- ??
--           (OneOf _)     -> text "fromElem" <+> cvo
--           (String)       -> text "fromText" <+> cvo
--           (Any)          -> text "fromElem" <+> cvo
--           (Defined _)    -> text "fromElem" <+> cvo
--         )
--     failpat sts =
--         let fp st =
--                 case st of
--                   (Maybe _)   -> text "Nothing"
--                   (List _)    -> text "[]"
--                   (List1 _)   -> text "_"
--                   (Tuple _)  -> text "_"
--                   (OneOf _)  -> text "_"
--                   (String)    -> text "_"
--                   (Any)       -> text "_"
--                   (Defined _) -> text "_"
--         in parens (hcat (intersperse comma (map fp sts++[text "_"])))
--     succpat sts vs =
--         let sp st v =
--                 case st of
--                   (Maybe _)   -> v
--                   (List _)    -> v
--                   (List1 _)   -> text "Just" <+> v
--                   (Tuple _)  -> text "Just" <+> v
--                   (OneOf _)  -> text "Just" <+> v
--                   (String)    -> text "Just" <+> v
--                   (Any)       -> text "Just" <+> v
--                   (Defined _) -> text "Just" <+> v
--         in parens (hcat (intersperse comma (zipWith sp sts vs++[rest])))

mkToAux :: Bool -> (Name,[StructType]) -> Doc
mkToAux mixattrs (n,sts) =
    let vs = nameSupply sts
        attrs = if mixattrs then text "as" else empty
    in
    text "toContents" <+> parens (mkCpat n attrs vs) <+> text "=" <+>
    mkToElem sts vs

mkToMult :: Name -> Doc -> Doc -> (Name,[StructType]) -> Doc
mkToMult tag attrpat attrexp (n,sts) =
    let vs = nameSupply sts
    in
    text "toContents" <+> parens (mkCpat n attrpat vs) <+> text "="
    $$ nest 4 (text "[CElem (Elem (N \"" <> ppXName tag <> text "\")"<+> attrexp
              <+> parens (mkToElem sts vs) <+> text ") ()]")

