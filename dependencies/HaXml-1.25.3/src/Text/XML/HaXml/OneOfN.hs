module Text.XML.HaXml.OneOfN where

import Text.XML.HaXml.XmlContent

-- | Somewhat of a nonsense - a choice of a single item.  But sometimes it
--   occurs in auto-generated code.
data OneOf1 a
    = OneOf1 a
    deriving (Eq,Show)

instance (HTypeable a)
    => HTypeable (OneOf1 a)
  where      toHType _ = Defined "OneOf1" [] []
  --         toHType m = Defined "OneOf1" [a] []
  --            where a = toHType $ (\ (OneOf1 a)->a) $ m

instance (XmlContent a)
    => XmlContent (OneOf1 a)
  where
    parseContents =
        (choice OneOf1
        $ fail "OneOf1")
    toContents (OneOf1 x) = toContents x

foldOneOf1 :: (a->z) -> 
               OneOf1 a
               -> z
foldOneOf1 a (OneOf1 z) = a z

----

-- | Equivalent to the Either type, but using the regular naming
--   scheme of this module.
data OneOf2 a b
    = OneOf2 a | TwoOf2 b
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b)
    => HTypeable (OneOf2 a b)
  where      toHType _ = Defined "OneOf2" [] []
  --         toHType m = Defined "OneOf2" [a,b] []
  --            where a = toHType $ (\ (OneOf2 a)->a) $ m
  --                  b = toHType $ (\ (TwoOf2 b)->b) $ m

instance (XmlContent a,XmlContent b)
    => XmlContent (OneOf2 a b)
  where
    parseContents =
        (choice OneOf2 $ choice TwoOf2
        $ fail "OneOf2")
    toContents (OneOf2 x) = toContents x
    toContents (TwoOf2 x) = toContents x

foldOneOf2 :: (a->z) -> (b->z) -> 
               OneOf2 a b
               -> z
foldOneOf2 a b (OneOf2 z) = a z
foldOneOf2 a b (TwoOf2 z) = b z

----
data OneOf3 a b c
    = OneOf3 a | TwoOf3 b | ThreeOf3 c
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c)
    => HTypeable (OneOf3 a b c)
  where      toHType _ = Defined "OneOf3" [] []

instance (XmlContent a,XmlContent b,XmlContent c)
    => XmlContent (OneOf3 a b c)
  where
    parseContents =
        (choice OneOf3 $ choice TwoOf3 $ choice ThreeOf3
        $ fail "OneOf3")
    toContents (OneOf3 x) = toContents x
    toContents (TwoOf3 x) = toContents x
    toContents (ThreeOf3 x) = toContents x

foldOneOf3 :: (a->z) -> (b->z) -> (c->z) -> 
               OneOf3 a b c
               -> z
foldOneOf3 a b c (OneOf3 z) = a z
foldOneOf3 a b c (TwoOf3 z) = b z
foldOneOf3 a b c (ThreeOf3 z) = c z

----
data OneOf4 a b c d
    = OneOf4 a | TwoOf4 b | ThreeOf4 c | FourOf4 d
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d)
    => HTypeable (OneOf4 a b c d)
  where      toHType _ = Defined "OneOf4" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d)
    => XmlContent (OneOf4 a b c d)
  where
    parseContents =
        (choice OneOf4 $ choice TwoOf4 $ choice ThreeOf4 $ choice FourOf4
        $ fail "OneOf4")
    toContents (OneOf4 x) = toContents x
    toContents (TwoOf4 x) = toContents x
    toContents (ThreeOf4 x) = toContents x
    toContents (FourOf4 x) = toContents x

foldOneOf4 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> 
               OneOf4 a b c d
               -> z
foldOneOf4 a b c d (OneOf4 z) = a z
foldOneOf4 a b c d (TwoOf4 z) = b z
foldOneOf4 a b c d (ThreeOf4 z) = c z
foldOneOf4 a b c d (FourOf4 z) = d z

----
data OneOf5 a b c d e
    = OneOf5 a | TwoOf5 b | ThreeOf5 c | FourOf5 d | FiveOf5 e
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d,HTypeable e)
    => HTypeable (OneOf5 a b c d e)
  where      toHType _ = Defined "OneOf5" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e)
    => XmlContent (OneOf5 a b c d e)
  where
    parseContents =
        (choice OneOf5 $ choice TwoOf5 $ choice ThreeOf5 $ choice FourOf5
        $ choice FiveOf5
        $ fail "OneOf5")
    toContents (OneOf5 x) = toContents x
    toContents (TwoOf5 x) = toContents x
    toContents (ThreeOf5 x) = toContents x
    toContents (FourOf5 x) = toContents x
    toContents (FiveOf5 x) = toContents x

foldOneOf5 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> (e->z) -> 
               OneOf5 a b c d e
               -> z
foldOneOf5 a b c d e (OneOf5 z) = a z
foldOneOf5 a b c d e (TwoOf5 z) = b z
foldOneOf5 a b c d e (ThreeOf5 z) = c z
foldOneOf5 a b c d e (FourOf5 z) = d z
foldOneOf5 a b c d e (FiveOf5 z) = e z

----
data OneOf6 a b c d e f
    = OneOf6 a | TwoOf6 b | ThreeOf6 c | FourOf6 d | FiveOf6 e | SixOf6 f
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d,HTypeable e
         ,HTypeable f)
    => HTypeable (OneOf6 a b c d e f)
  where      toHType _ = Defined "OneOf6" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
         ,XmlContent f)
    => XmlContent (OneOf6 a b c d e f)
  where
    parseContents =
        (choice OneOf6 $ choice TwoOf6 $ choice ThreeOf6 $ choice FourOf6
        $ choice FiveOf6 $ choice SixOf6
        $ fail "OneOf6")
    toContents (OneOf6 x) = toContents x
    toContents (TwoOf6 x) = toContents x
    toContents (ThreeOf6 x) = toContents x
    toContents (FourOf6 x) = toContents x
    toContents (FiveOf6 x) = toContents x
    toContents (SixOf6 x) = toContents x

foldOneOf6 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> (e->z) -> (f->z) -> 
               OneOf6 a b c d e f
               -> z
foldOneOf6 a b c d e f (OneOf6 z) = a z
foldOneOf6 a b c d e f (TwoOf6 z) = b z
foldOneOf6 a b c d e f (ThreeOf6 z) = c z
foldOneOf6 a b c d e f (FourOf6 z) = d z
foldOneOf6 a b c d e f (FiveOf6 z) = e z
foldOneOf6 a b c d e f (SixOf6 z) = f z

----
data OneOf7 a b c d e f g
    = OneOf7 a | TwoOf7 b | ThreeOf7 c | FourOf7 d | FiveOf7 e | SixOf7 f
    | SevenOf7 g
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d,HTypeable e
         ,HTypeable f,HTypeable g)
    => HTypeable (OneOf7 a b c d e f g)
  where      toHType _ = Defined "OneOf7" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
         ,XmlContent f,XmlContent g)
    => XmlContent (OneOf7 a b c d e f g)
  where
    parseContents =
        (choice OneOf7 $ choice TwoOf7 $ choice ThreeOf7 $ choice FourOf7
        $ choice FiveOf7 $ choice SixOf7 $ choice SevenOf7
        $ fail "OneOf7")
    toContents (OneOf7 x) = toContents x
    toContents (TwoOf7 x) = toContents x
    toContents (ThreeOf7 x) = toContents x
    toContents (FourOf7 x) = toContents x
    toContents (FiveOf7 x) = toContents x
    toContents (SixOf7 x) = toContents x
    toContents (SevenOf7 x) = toContents x

foldOneOf7 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> (e->z) -> (f->z) -> 
               (g->z) -> 
               OneOf7 a b c d e f g
               -> z
foldOneOf7 a b c d e f g (OneOf7 z) = a z
foldOneOf7 a b c d e f g (TwoOf7 z) = b z
foldOneOf7 a b c d e f g (ThreeOf7 z) = c z
foldOneOf7 a b c d e f g (FourOf7 z) = d z
foldOneOf7 a b c d e f g (FiveOf7 z) = e z
foldOneOf7 a b c d e f g (SixOf7 z) = f z
foldOneOf7 a b c d e f g (SevenOf7 z) = g z

----
data OneOf8 a b c d e f g h
    = OneOf8 a | TwoOf8 b | ThreeOf8 c | FourOf8 d | FiveOf8 e | SixOf8 f
    | SevenOf8 g | EightOf8 h
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d,HTypeable e
         ,HTypeable f,HTypeable g,HTypeable h)
    => HTypeable (OneOf8 a b c d e f g h)
  where      toHType _ = Defined "OneOf8" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
         ,XmlContent f,XmlContent g,XmlContent h)
    => XmlContent (OneOf8 a b c d e f g h)
  where
    parseContents =
        (choice OneOf8 $ choice TwoOf8 $ choice ThreeOf8 $ choice FourOf8
        $ choice FiveOf8 $ choice SixOf8 $ choice SevenOf8 $ choice EightOf8
        $ fail "OneOf8")
    toContents (OneOf8 x) = toContents x
    toContents (TwoOf8 x) = toContents x
    toContents (ThreeOf8 x) = toContents x
    toContents (FourOf8 x) = toContents x
    toContents (FiveOf8 x) = toContents x
    toContents (SixOf8 x) = toContents x
    toContents (SevenOf8 x) = toContents x
    toContents (EightOf8 x) = toContents x

foldOneOf8 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> (e->z) -> (f->z) -> 
               (g->z) -> (h->z) -> 
               OneOf8 a b c d e f g h
               -> z
foldOneOf8 a b c d e f g h (OneOf8 z) = a z
foldOneOf8 a b c d e f g h (TwoOf8 z) = b z
foldOneOf8 a b c d e f g h (ThreeOf8 z) = c z
foldOneOf8 a b c d e f g h (FourOf8 z) = d z
foldOneOf8 a b c d e f g h (FiveOf8 z) = e z
foldOneOf8 a b c d e f g h (SixOf8 z) = f z
foldOneOf8 a b c d e f g h (SevenOf8 z) = g z
foldOneOf8 a b c d e f g h (EightOf8 z) = h z

----
data OneOf9 a b c d e f g h i
    = OneOf9 a | TwoOf9 b | ThreeOf9 c | FourOf9 d | FiveOf9 e | SixOf9 f
    | SevenOf9 g | EightOf9 h | NineOf9 i
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d,HTypeable e
         ,HTypeable f,HTypeable g,HTypeable h,HTypeable i)
    => HTypeable (OneOf9 a b c d e f g h i)
  where      toHType _ = Defined "OneOf9" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
         ,XmlContent f,XmlContent g,XmlContent h,XmlContent i)
    => XmlContent (OneOf9 a b c d e f g h i)
  where
    parseContents =
        (choice OneOf9 $ choice TwoOf9 $ choice ThreeOf9 $ choice FourOf9
        $ choice FiveOf9 $ choice SixOf9 $ choice SevenOf9 $ choice EightOf9
        $ choice NineOf9
        $ fail "OneOf9")
    toContents (OneOf9 x) = toContents x
    toContents (TwoOf9 x) = toContents x
    toContents (ThreeOf9 x) = toContents x
    toContents (FourOf9 x) = toContents x
    toContents (FiveOf9 x) = toContents x
    toContents (SixOf9 x) = toContents x
    toContents (SevenOf9 x) = toContents x
    toContents (EightOf9 x) = toContents x
    toContents (NineOf9 x) = toContents x

foldOneOf9 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> (e->z) -> (f->z) -> 
               (g->z) -> (h->z) -> (i->z) -> 
               OneOf9 a b c d e f g h i
               -> z
foldOneOf9 a b c d e f g h i (OneOf9 z) = a z
foldOneOf9 a b c d e f g h i (TwoOf9 z) = b z
foldOneOf9 a b c d e f g h i (ThreeOf9 z) = c z
foldOneOf9 a b c d e f g h i (FourOf9 z) = d z
foldOneOf9 a b c d e f g h i (FiveOf9 z) = e z
foldOneOf9 a b c d e f g h i (SixOf9 z) = f z
foldOneOf9 a b c d e f g h i (SevenOf9 z) = g z
foldOneOf9 a b c d e f g h i (EightOf9 z) = h z
foldOneOf9 a b c d e f g h i (NineOf9 z) = i z

----
data OneOf10 a b c d e f g h i j
    = OneOf10 a | TwoOf10 b | ThreeOf10 c | FourOf10 d | FiveOf10 e
    | SixOf10 f | SevenOf10 g | EightOf10 h | NineOf10 i | TenOf10 j
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d,HTypeable e
         ,HTypeable f,HTypeable g,HTypeable h,HTypeable i,HTypeable j)
    => HTypeable (OneOf10 a b c d e f g h i j)
  where      toHType _ = Defined "OneOf10" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
         ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j)
    => XmlContent (OneOf10 a b c d e f g h i j)
  where
    parseContents =
        (choice OneOf10 $ choice TwoOf10 $ choice ThreeOf10 $ choice FourOf10
        $ choice FiveOf10 $ choice SixOf10 $ choice SevenOf10
        $ choice EightOf10 $ choice NineOf10 $ choice TenOf10
        $ fail "OneOf10")
    toContents (OneOf10 x) = toContents x
    toContents (TwoOf10 x) = toContents x
    toContents (ThreeOf10 x) = toContents x
    toContents (FourOf10 x) = toContents x
    toContents (FiveOf10 x) = toContents x
    toContents (SixOf10 x) = toContents x
    toContents (SevenOf10 x) = toContents x
    toContents (EightOf10 x) = toContents x
    toContents (NineOf10 x) = toContents x
    toContents (TenOf10 x) = toContents x

foldOneOf10 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> (e->z) -> (f->z) -> 
               (g->z) -> (h->z) -> (i->z) -> (j->z) -> 
               OneOf10 a b c d e f g h i j
               -> z
foldOneOf10 a b c d e f g h i j (OneOf10 z) = a z
foldOneOf10 a b c d e f g h i j (TwoOf10 z) = b z
foldOneOf10 a b c d e f g h i j (ThreeOf10 z) = c z
foldOneOf10 a b c d e f g h i j (FourOf10 z) = d z
foldOneOf10 a b c d e f g h i j (FiveOf10 z) = e z
foldOneOf10 a b c d e f g h i j (SixOf10 z) = f z
foldOneOf10 a b c d e f g h i j (SevenOf10 z) = g z
foldOneOf10 a b c d e f g h i j (EightOf10 z) = h z
foldOneOf10 a b c d e f g h i j (NineOf10 z) = i z
foldOneOf10 a b c d e f g h i j (TenOf10 z) = j z

----
data OneOf11 a b c d e f g h i j k
    = OneOf11 a | TwoOf11 b | ThreeOf11 c | FourOf11 d | FiveOf11 e
    | SixOf11 f | SevenOf11 g | EightOf11 h | NineOf11 i | TenOf11 j
    | ElevenOf11 k
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d,HTypeable e
         ,HTypeable f,HTypeable g,HTypeable h,HTypeable i,HTypeable j
         ,HTypeable k)
    => HTypeable (OneOf11 a b c d e f g h i j k)
  where      toHType _ = Defined "OneOf11" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
         ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
         ,XmlContent k)
    => XmlContent (OneOf11 a b c d e f g h i j k)
  where
    parseContents =
        (choice OneOf11 $ choice TwoOf11 $ choice ThreeOf11 $ choice FourOf11
        $ choice FiveOf11 $ choice SixOf11 $ choice SevenOf11
        $ choice EightOf11 $ choice NineOf11 $ choice TenOf11
        $ choice ElevenOf11
        $ fail "OneOf11")
    toContents (OneOf11 x) = toContents x
    toContents (TwoOf11 x) = toContents x
    toContents (ThreeOf11 x) = toContents x
    toContents (FourOf11 x) = toContents x
    toContents (FiveOf11 x) = toContents x
    toContents (SixOf11 x) = toContents x
    toContents (SevenOf11 x) = toContents x
    toContents (EightOf11 x) = toContents x
    toContents (NineOf11 x) = toContents x
    toContents (TenOf11 x) = toContents x
    toContents (ElevenOf11 x) = toContents x

foldOneOf11 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> (e->z) -> (f->z) -> 
               (g->z) -> (h->z) -> (i->z) -> (j->z) -> (k->z) -> 
               OneOf11 a b c d e f g h i j k
               -> z
foldOneOf11 a b c d e f g h i j k (OneOf11 z) = a z
foldOneOf11 a b c d e f g h i j k (TwoOf11 z) = b z
foldOneOf11 a b c d e f g h i j k (ThreeOf11 z) = c z
foldOneOf11 a b c d e f g h i j k (FourOf11 z) = d z
foldOneOf11 a b c d e f g h i j k (FiveOf11 z) = e z
foldOneOf11 a b c d e f g h i j k (SixOf11 z) = f z
foldOneOf11 a b c d e f g h i j k (SevenOf11 z) = g z
foldOneOf11 a b c d e f g h i j k (EightOf11 z) = h z
foldOneOf11 a b c d e f g h i j k (NineOf11 z) = i z
foldOneOf11 a b c d e f g h i j k (TenOf11 z) = j z
foldOneOf11 a b c d e f g h i j k (ElevenOf11 z) = k z

----
data OneOf12 a b c d e f g h i j k l
    = OneOf12 a | TwoOf12 b | ThreeOf12 c | FourOf12 d | FiveOf12 e
    | SixOf12 f | SevenOf12 g | EightOf12 h | NineOf12 i | TenOf12 j
    | ElevenOf12 k | TwelveOf12 l
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d,HTypeable e
         ,HTypeable f,HTypeable g,HTypeable h,HTypeable i,HTypeable j
         ,HTypeable k,HTypeable l)
    => HTypeable (OneOf12 a b c d e f g h i j k l)
  where      toHType _ = Defined "OneOf12" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
         ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
         ,XmlContent k,XmlContent l)
    => XmlContent (OneOf12 a b c d e f g h i j k l)
  where
    parseContents =
        (choice OneOf12 $ choice TwoOf12 $ choice ThreeOf12 $ choice FourOf12
        $ choice FiveOf12 $ choice SixOf12 $ choice SevenOf12
        $ choice EightOf12 $ choice NineOf12 $ choice TenOf12
        $ choice ElevenOf12 $ choice TwelveOf12
        $ fail "OneOf12")
    toContents (OneOf12 x) = toContents x
    toContents (TwoOf12 x) = toContents x
    toContents (ThreeOf12 x) = toContents x
    toContents (FourOf12 x) = toContents x
    toContents (FiveOf12 x) = toContents x
    toContents (SixOf12 x) = toContents x
    toContents (SevenOf12 x) = toContents x
    toContents (EightOf12 x) = toContents x
    toContents (NineOf12 x) = toContents x
    toContents (TenOf12 x) = toContents x
    toContents (ElevenOf12 x) = toContents x
    toContents (TwelveOf12 x) = toContents x

foldOneOf12 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> (e->z) -> (f->z) -> 
               (g->z) -> (h->z) -> (i->z) -> (j->z) -> (k->z) -> (l->z) -> 
               OneOf12 a b c d e f g h i j k l
               -> z
foldOneOf12 a b c d e f g h i j k l (OneOf12 z) = a z
foldOneOf12 a b c d e f g h i j k l (TwoOf12 z) = b z
foldOneOf12 a b c d e f g h i j k l (ThreeOf12 z) = c z
foldOneOf12 a b c d e f g h i j k l (FourOf12 z) = d z
foldOneOf12 a b c d e f g h i j k l (FiveOf12 z) = e z
foldOneOf12 a b c d e f g h i j k l (SixOf12 z) = f z
foldOneOf12 a b c d e f g h i j k l (SevenOf12 z) = g z
foldOneOf12 a b c d e f g h i j k l (EightOf12 z) = h z
foldOneOf12 a b c d e f g h i j k l (NineOf12 z) = i z
foldOneOf12 a b c d e f g h i j k l (TenOf12 z) = j z
foldOneOf12 a b c d e f g h i j k l (ElevenOf12 z) = k z
foldOneOf12 a b c d e f g h i j k l (TwelveOf12 z) = l z

----
data OneOf13 a b c d e f g h i j k l m
    = OneOf13 a | TwoOf13 b | ThreeOf13 c | FourOf13 d | FiveOf13 e
    | SixOf13 f | SevenOf13 g | EightOf13 h | NineOf13 i | TenOf13 j
    | ElevenOf13 k | TwelveOf13 l | ThirteenOf13 m
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d,HTypeable e
         ,HTypeable f,HTypeable g,HTypeable h,HTypeable i,HTypeable j
         ,HTypeable k,HTypeable l,HTypeable m)
    => HTypeable (OneOf13 a b c d e f g h i j k l m)
  where      toHType _ = Defined "OneOf13" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
         ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
         ,XmlContent k,XmlContent l,XmlContent m)
    => XmlContent (OneOf13 a b c d e f g h i j k l m)
  where
    parseContents =
        (choice OneOf13 $ choice TwoOf13 $ choice ThreeOf13 $ choice FourOf13
        $ choice FiveOf13 $ choice SixOf13 $ choice SevenOf13
        $ choice EightOf13 $ choice NineOf13 $ choice TenOf13
        $ choice ElevenOf13 $ choice TwelveOf13 $ choice ThirteenOf13
        $ fail "OneOf13")
    toContents (OneOf13 x) = toContents x
    toContents (TwoOf13 x) = toContents x
    toContents (ThreeOf13 x) = toContents x
    toContents (FourOf13 x) = toContents x
    toContents (FiveOf13 x) = toContents x
    toContents (SixOf13 x) = toContents x
    toContents (SevenOf13 x) = toContents x
    toContents (EightOf13 x) = toContents x
    toContents (NineOf13 x) = toContents x
    toContents (TenOf13 x) = toContents x
    toContents (ElevenOf13 x) = toContents x
    toContents (TwelveOf13 x) = toContents x
    toContents (ThirteenOf13 x) = toContents x

foldOneOf13 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> (e->z) -> (f->z) -> 
               (g->z) -> (h->z) -> (i->z) -> (j->z) -> (k->z) -> (l->z) -> 
               (m->z) -> 
               OneOf13 a b c d e f g h i j k l m
               -> z
foldOneOf13 a b c d e f g h i j k l m (OneOf13 z) = a z
foldOneOf13 a b c d e f g h i j k l m (TwoOf13 z) = b z
foldOneOf13 a b c d e f g h i j k l m (ThreeOf13 z) = c z
foldOneOf13 a b c d e f g h i j k l m (FourOf13 z) = d z
foldOneOf13 a b c d e f g h i j k l m (FiveOf13 z) = e z
foldOneOf13 a b c d e f g h i j k l m (SixOf13 z) = f z
foldOneOf13 a b c d e f g h i j k l m (SevenOf13 z) = g z
foldOneOf13 a b c d e f g h i j k l m (EightOf13 z) = h z
foldOneOf13 a b c d e f g h i j k l m (NineOf13 z) = i z
foldOneOf13 a b c d e f g h i j k l m (TenOf13 z) = j z
foldOneOf13 a b c d e f g h i j k l m (ElevenOf13 z) = k z
foldOneOf13 a b c d e f g h i j k l m (TwelveOf13 z) = l z
foldOneOf13 a b c d e f g h i j k l m (ThirteenOf13 z) = m z

----
data OneOf14 a b c d e f g h i j k l m n
    = OneOf14 a | TwoOf14 b | ThreeOf14 c | FourOf14 d | FiveOf14 e
    | SixOf14 f | SevenOf14 g | EightOf14 h | NineOf14 i | TenOf14 j
    | ElevenOf14 k | TwelveOf14 l | ThirteenOf14 m | FourteenOf14 n
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d,HTypeable e
         ,HTypeable f,HTypeable g,HTypeable h,HTypeable i,HTypeable j
         ,HTypeable k,HTypeable l,HTypeable m,HTypeable n)
    => HTypeable (OneOf14 a b c d e f g h i j k l m n)
  where      toHType _ = Defined "OneOf14" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
         ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
         ,XmlContent k,XmlContent l,XmlContent m,XmlContent n)
    => XmlContent (OneOf14 a b c d e f g h i j k l m n)
  where
    parseContents =
        (choice OneOf14 $ choice TwoOf14 $ choice ThreeOf14 $ choice FourOf14
        $ choice FiveOf14 $ choice SixOf14 $ choice SevenOf14
        $ choice EightOf14 $ choice NineOf14 $ choice TenOf14
        $ choice ElevenOf14 $ choice TwelveOf14 $ choice ThirteenOf14
        $ choice FourteenOf14
        $ fail "OneOf14")
    toContents (OneOf14 x) = toContents x
    toContents (TwoOf14 x) = toContents x
    toContents (ThreeOf14 x) = toContents x
    toContents (FourOf14 x) = toContents x
    toContents (FiveOf14 x) = toContents x
    toContents (SixOf14 x) = toContents x
    toContents (SevenOf14 x) = toContents x
    toContents (EightOf14 x) = toContents x
    toContents (NineOf14 x) = toContents x
    toContents (TenOf14 x) = toContents x
    toContents (ElevenOf14 x) = toContents x
    toContents (TwelveOf14 x) = toContents x
    toContents (ThirteenOf14 x) = toContents x
    toContents (FourteenOf14 x) = toContents x

foldOneOf14 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> (e->z) -> (f->z) -> 
               (g->z) -> (h->z) -> (i->z) -> (j->z) -> (k->z) -> (l->z) -> 
               (m->z) -> (n->z) -> 
               OneOf14 a b c d e f g h i j k l m n
               -> z
foldOneOf14 a b c d e f g h i j k l m n (OneOf14 z) = a z
foldOneOf14 a b c d e f g h i j k l m n (TwoOf14 z) = b z
foldOneOf14 a b c d e f g h i j k l m n (ThreeOf14 z) = c z
foldOneOf14 a b c d e f g h i j k l m n (FourOf14 z) = d z
foldOneOf14 a b c d e f g h i j k l m n (FiveOf14 z) = e z
foldOneOf14 a b c d e f g h i j k l m n (SixOf14 z) = f z
foldOneOf14 a b c d e f g h i j k l m n (SevenOf14 z) = g z
foldOneOf14 a b c d e f g h i j k l m n (EightOf14 z) = h z
foldOneOf14 a b c d e f g h i j k l m n (NineOf14 z) = i z
foldOneOf14 a b c d e f g h i j k l m n (TenOf14 z) = j z
foldOneOf14 a b c d e f g h i j k l m n (ElevenOf14 z) = k z
foldOneOf14 a b c d e f g h i j k l m n (TwelveOf14 z) = l z
foldOneOf14 a b c d e f g h i j k l m n (ThirteenOf14 z) = m z
foldOneOf14 a b c d e f g h i j k l m n (FourteenOf14 z) = n z

----
data OneOf15 a b c d e f g h i j k l m n o
    = OneOf15 a | TwoOf15 b | ThreeOf15 c | FourOf15 d | FiveOf15 e
    | SixOf15 f | SevenOf15 g | EightOf15 h | NineOf15 i | TenOf15 j
    | ElevenOf15 k | TwelveOf15 l | ThirteenOf15 m | FourteenOf15 n
    | FifteenOf15 o
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d,HTypeable e
         ,HTypeable f,HTypeable g,HTypeable h,HTypeable i,HTypeable j
         ,HTypeable k,HTypeable l,HTypeable m,HTypeable n,HTypeable o)
    => HTypeable (OneOf15 a b c d e f g h i j k l m n o)
  where      toHType _ = Defined "OneOf15" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
         ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
         ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o)
    => XmlContent (OneOf15 a b c d e f g h i j k l m n o)
  where
    parseContents =
        (choice OneOf15 $ choice TwoOf15 $ choice ThreeOf15 $ choice FourOf15
        $ choice FiveOf15 $ choice SixOf15 $ choice SevenOf15
        $ choice EightOf15 $ choice NineOf15 $ choice TenOf15
        $ choice ElevenOf15 $ choice TwelveOf15 $ choice ThirteenOf15
        $ choice FourteenOf15 $ choice FifteenOf15
        $ fail "OneOf15")
    toContents (OneOf15 x) = toContents x
    toContents (TwoOf15 x) = toContents x
    toContents (ThreeOf15 x) = toContents x
    toContents (FourOf15 x) = toContents x
    toContents (FiveOf15 x) = toContents x
    toContents (SixOf15 x) = toContents x
    toContents (SevenOf15 x) = toContents x
    toContents (EightOf15 x) = toContents x
    toContents (NineOf15 x) = toContents x
    toContents (TenOf15 x) = toContents x
    toContents (ElevenOf15 x) = toContents x
    toContents (TwelveOf15 x) = toContents x
    toContents (ThirteenOf15 x) = toContents x
    toContents (FourteenOf15 x) = toContents x
    toContents (FifteenOf15 x) = toContents x

foldOneOf15 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> (e->z) -> (f->z) -> 
               (g->z) -> (h->z) -> (i->z) -> (j->z) -> (k->z) -> (l->z) -> 
               (m->z) -> (n->z) -> (o->z) -> 
               OneOf15 a b c d e f g h i j k l m n o
               -> z
foldOneOf15 a b c d e f g h i j k l m n o (OneOf15 z) = a z
foldOneOf15 a b c d e f g h i j k l m n o (TwoOf15 z) = b z
foldOneOf15 a b c d e f g h i j k l m n o (ThreeOf15 z) = c z
foldOneOf15 a b c d e f g h i j k l m n o (FourOf15 z) = d z
foldOneOf15 a b c d e f g h i j k l m n o (FiveOf15 z) = e z
foldOneOf15 a b c d e f g h i j k l m n o (SixOf15 z) = f z
foldOneOf15 a b c d e f g h i j k l m n o (SevenOf15 z) = g z
foldOneOf15 a b c d e f g h i j k l m n o (EightOf15 z) = h z
foldOneOf15 a b c d e f g h i j k l m n o (NineOf15 z) = i z
foldOneOf15 a b c d e f g h i j k l m n o (TenOf15 z) = j z
foldOneOf15 a b c d e f g h i j k l m n o (ElevenOf15 z) = k z
foldOneOf15 a b c d e f g h i j k l m n o (TwelveOf15 z) = l z
foldOneOf15 a b c d e f g h i j k l m n o (ThirteenOf15 z) = m z
foldOneOf15 a b c d e f g h i j k l m n o (FourteenOf15 z) = n z
foldOneOf15 a b c d e f g h i j k l m n o (FifteenOf15 z) = o z

----
data OneOf16 a b c d e f g h i j k l m n o p
    = OneOf16 a | TwoOf16 b | ThreeOf16 c | FourOf16 d | FiveOf16 e
    | SixOf16 f | SevenOf16 g | EightOf16 h | NineOf16 i | TenOf16 j
    | ElevenOf16 k | TwelveOf16 l | ThirteenOf16 m | FourteenOf16 n
    | FifteenOf16 o | SixteenOf16 p
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d,HTypeable e
         ,HTypeable f,HTypeable g,HTypeable h,HTypeable i,HTypeable j
         ,HTypeable k,HTypeable l,HTypeable m,HTypeable n,HTypeable o
         ,HTypeable p)
    => HTypeable (OneOf16 a b c d e f g h i j k l m n o p)
  where      toHType _ = Defined "OneOf16" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
         ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
         ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
         ,XmlContent p)
    => XmlContent (OneOf16 a b c d e f g h i j k l m n o p)
  where
    parseContents =
        (choice OneOf16 $ choice TwoOf16 $ choice ThreeOf16 $ choice FourOf16
        $ choice FiveOf16 $ choice SixOf16 $ choice SevenOf16
        $ choice EightOf16 $ choice NineOf16 $ choice TenOf16
        $ choice ElevenOf16 $ choice TwelveOf16 $ choice ThirteenOf16
        $ choice FourteenOf16 $ choice FifteenOf16 $ choice SixteenOf16
        $ fail "OneOf16")
    toContents (OneOf16 x) = toContents x
    toContents (TwoOf16 x) = toContents x
    toContents (ThreeOf16 x) = toContents x
    toContents (FourOf16 x) = toContents x
    toContents (FiveOf16 x) = toContents x
    toContents (SixOf16 x) = toContents x
    toContents (SevenOf16 x) = toContents x
    toContents (EightOf16 x) = toContents x
    toContents (NineOf16 x) = toContents x
    toContents (TenOf16 x) = toContents x
    toContents (ElevenOf16 x) = toContents x
    toContents (TwelveOf16 x) = toContents x
    toContents (ThirteenOf16 x) = toContents x
    toContents (FourteenOf16 x) = toContents x
    toContents (FifteenOf16 x) = toContents x
    toContents (SixteenOf16 x) = toContents x

foldOneOf16 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> (e->z) -> (f->z) -> 
               (g->z) -> (h->z) -> (i->z) -> (j->z) -> (k->z) -> (l->z) -> 
               (m->z) -> (n->z) -> (o->z) -> (p->z) -> 
               OneOf16 a b c d e f g h i j k l m n o p
               -> z
foldOneOf16 a b c d e f g h i j k l m n o p (OneOf16 z) = a z
foldOneOf16 a b c d e f g h i j k l m n o p (TwoOf16 z) = b z
foldOneOf16 a b c d e f g h i j k l m n o p (ThreeOf16 z) = c z
foldOneOf16 a b c d e f g h i j k l m n o p (FourOf16 z) = d z
foldOneOf16 a b c d e f g h i j k l m n o p (FiveOf16 z) = e z
foldOneOf16 a b c d e f g h i j k l m n o p (SixOf16 z) = f z
foldOneOf16 a b c d e f g h i j k l m n o p (SevenOf16 z) = g z
foldOneOf16 a b c d e f g h i j k l m n o p (EightOf16 z) = h z
foldOneOf16 a b c d e f g h i j k l m n o p (NineOf16 z) = i z
foldOneOf16 a b c d e f g h i j k l m n o p (TenOf16 z) = j z
foldOneOf16 a b c d e f g h i j k l m n o p (ElevenOf16 z) = k z
foldOneOf16 a b c d e f g h i j k l m n o p (TwelveOf16 z) = l z
foldOneOf16 a b c d e f g h i j k l m n o p (ThirteenOf16 z) = m z
foldOneOf16 a b c d e f g h i j k l m n o p (FourteenOf16 z) = n z
foldOneOf16 a b c d e f g h i j k l m n o p (FifteenOf16 z) = o z
foldOneOf16 a b c d e f g h i j k l m n o p (SixteenOf16 z) = p z

----
data OneOf17 a b c d e f g h i j k l m n o p q
    = OneOf17 a | TwoOf17 b | ThreeOf17 c | FourOf17 d | FiveOf17 e
    | SixOf17 f | SevenOf17 g | EightOf17 h | NineOf17 i | TenOf17 j
    | ElevenOf17 k | TwelveOf17 l | ThirteenOf17 m | FourteenOf17 n
    | FifteenOf17 o | SixteenOf17 p | SeventeenOf17 q
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d,HTypeable e
         ,HTypeable f,HTypeable g,HTypeable h,HTypeable i,HTypeable j
         ,HTypeable k,HTypeable l,HTypeable m,HTypeable n,HTypeable o
         ,HTypeable p,HTypeable q)
    => HTypeable (OneOf17 a b c d e f g h i j k l m n o p q)
  where      toHType _ = Defined "OneOf17" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
         ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
         ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
         ,XmlContent p,XmlContent q)
    => XmlContent (OneOf17 a b c d e f g h i j k l m n o p q)
  where
    parseContents =
        (choice OneOf17 $ choice TwoOf17 $ choice ThreeOf17 $ choice FourOf17
        $ choice FiveOf17 $ choice SixOf17 $ choice SevenOf17
        $ choice EightOf17 $ choice NineOf17 $ choice TenOf17
        $ choice ElevenOf17 $ choice TwelveOf17 $ choice ThirteenOf17
        $ choice FourteenOf17 $ choice FifteenOf17 $ choice SixteenOf17
        $ choice SeventeenOf17
        $ fail "OneOf17")
    toContents (OneOf17 x) = toContents x
    toContents (TwoOf17 x) = toContents x
    toContents (ThreeOf17 x) = toContents x
    toContents (FourOf17 x) = toContents x
    toContents (FiveOf17 x) = toContents x
    toContents (SixOf17 x) = toContents x
    toContents (SevenOf17 x) = toContents x
    toContents (EightOf17 x) = toContents x
    toContents (NineOf17 x) = toContents x
    toContents (TenOf17 x) = toContents x
    toContents (ElevenOf17 x) = toContents x
    toContents (TwelveOf17 x) = toContents x
    toContents (ThirteenOf17 x) = toContents x
    toContents (FourteenOf17 x) = toContents x
    toContents (FifteenOf17 x) = toContents x
    toContents (SixteenOf17 x) = toContents x
    toContents (SeventeenOf17 x) = toContents x

foldOneOf17 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> (e->z) -> (f->z) -> 
               (g->z) -> (h->z) -> (i->z) -> (j->z) -> (k->z) -> (l->z) -> 
               (m->z) -> (n->z) -> (o->z) -> (p->z) -> (q->z) -> 
               OneOf17 a b c d e f g h i j k l m n o p q
               -> z
foldOneOf17 a b c d e f g h i j k l m n o p q (OneOf17 z) = a z
foldOneOf17 a b c d e f g h i j k l m n o p q (TwoOf17 z) = b z
foldOneOf17 a b c d e f g h i j k l m n o p q (ThreeOf17 z) = c z
foldOneOf17 a b c d e f g h i j k l m n o p q (FourOf17 z) = d z
foldOneOf17 a b c d e f g h i j k l m n o p q (FiveOf17 z) = e z
foldOneOf17 a b c d e f g h i j k l m n o p q (SixOf17 z) = f z
foldOneOf17 a b c d e f g h i j k l m n o p q (SevenOf17 z) = g z
foldOneOf17 a b c d e f g h i j k l m n o p q (EightOf17 z) = h z
foldOneOf17 a b c d e f g h i j k l m n o p q (NineOf17 z) = i z
foldOneOf17 a b c d e f g h i j k l m n o p q (TenOf17 z) = j z
foldOneOf17 a b c d e f g h i j k l m n o p q (ElevenOf17 z) = k z
foldOneOf17 a b c d e f g h i j k l m n o p q (TwelveOf17 z) = l z
foldOneOf17 a b c d e f g h i j k l m n o p q (ThirteenOf17 z) = m z
foldOneOf17 a b c d e f g h i j k l m n o p q (FourteenOf17 z) = n z
foldOneOf17 a b c d e f g h i j k l m n o p q (FifteenOf17 z) = o z
foldOneOf17 a b c d e f g h i j k l m n o p q (SixteenOf17 z) = p z
foldOneOf17 a b c d e f g h i j k l m n o p q (SeventeenOf17 z) = q z

----
data OneOf18 a b c d e f g h i j k l m n o p q r
    = OneOf18 a | TwoOf18 b | ThreeOf18 c | FourOf18 d | FiveOf18 e
    | SixOf18 f | SevenOf18 g | EightOf18 h | NineOf18 i | TenOf18 j
    | ElevenOf18 k | TwelveOf18 l | ThirteenOf18 m | FourteenOf18 n
    | FifteenOf18 o | SixteenOf18 p | SeventeenOf18 q | EighteenOf18 r
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d,HTypeable e
         ,HTypeable f,HTypeable g,HTypeable h,HTypeable i,HTypeable j
         ,HTypeable k,HTypeable l,HTypeable m,HTypeable n,HTypeable o
         ,HTypeable p,HTypeable q,HTypeable r)
    => HTypeable (OneOf18 a b c d e f g h i j k l m n o p q r)
  where      toHType _ = Defined "OneOf18" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
         ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
         ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
         ,XmlContent p,XmlContent q,XmlContent r)
    => XmlContent (OneOf18 a b c d e f g h i j k l m n o p q r)
  where
    parseContents =
        (choice OneOf18 $ choice TwoOf18 $ choice ThreeOf18 $ choice FourOf18
        $ choice FiveOf18 $ choice SixOf18 $ choice SevenOf18
        $ choice EightOf18 $ choice NineOf18 $ choice TenOf18
        $ choice ElevenOf18 $ choice TwelveOf18 $ choice ThirteenOf18
        $ choice FourteenOf18 $ choice FifteenOf18 $ choice SixteenOf18
        $ choice SeventeenOf18 $ choice EighteenOf18
        $ fail "OneOf18")
    toContents (OneOf18 x) = toContents x
    toContents (TwoOf18 x) = toContents x
    toContents (ThreeOf18 x) = toContents x
    toContents (FourOf18 x) = toContents x
    toContents (FiveOf18 x) = toContents x
    toContents (SixOf18 x) = toContents x
    toContents (SevenOf18 x) = toContents x
    toContents (EightOf18 x) = toContents x
    toContents (NineOf18 x) = toContents x
    toContents (TenOf18 x) = toContents x
    toContents (ElevenOf18 x) = toContents x
    toContents (TwelveOf18 x) = toContents x
    toContents (ThirteenOf18 x) = toContents x
    toContents (FourteenOf18 x) = toContents x
    toContents (FifteenOf18 x) = toContents x
    toContents (SixteenOf18 x) = toContents x
    toContents (SeventeenOf18 x) = toContents x
    toContents (EighteenOf18 x) = toContents x

foldOneOf18 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> (e->z) -> (f->z) -> 
               (g->z) -> (h->z) -> (i->z) -> (j->z) -> (k->z) -> (l->z) -> 
               (m->z) -> (n->z) -> (o->z) -> (p->z) -> (q->z) -> (r->z) -> 
               OneOf18 a b c d e f g h i j k l m n o p q r
               -> z
foldOneOf18 a b c d e f g h i j k l m n o p q r (OneOf18 z) = a z
foldOneOf18 a b c d e f g h i j k l m n o p q r (TwoOf18 z) = b z
foldOneOf18 a b c d e f g h i j k l m n o p q r (ThreeOf18 z) = c z
foldOneOf18 a b c d e f g h i j k l m n o p q r (FourOf18 z) = d z
foldOneOf18 a b c d e f g h i j k l m n o p q r (FiveOf18 z) = e z
foldOneOf18 a b c d e f g h i j k l m n o p q r (SixOf18 z) = f z
foldOneOf18 a b c d e f g h i j k l m n o p q r (SevenOf18 z) = g z
foldOneOf18 a b c d e f g h i j k l m n o p q r (EightOf18 z) = h z
foldOneOf18 a b c d e f g h i j k l m n o p q r (NineOf18 z) = i z
foldOneOf18 a b c d e f g h i j k l m n o p q r (TenOf18 z) = j z
foldOneOf18 a b c d e f g h i j k l m n o p q r (ElevenOf18 z) = k z
foldOneOf18 a b c d e f g h i j k l m n o p q r (TwelveOf18 z) = l z
foldOneOf18 a b c d e f g h i j k l m n o p q r (ThirteenOf18 z) = m z
foldOneOf18 a b c d e f g h i j k l m n o p q r (FourteenOf18 z) = n z
foldOneOf18 a b c d e f g h i j k l m n o p q r (FifteenOf18 z) = o z
foldOneOf18 a b c d e f g h i j k l m n o p q r (SixteenOf18 z) = p z
foldOneOf18 a b c d e f g h i j k l m n o p q r (SeventeenOf18 z) = q z
foldOneOf18 a b c d e f g h i j k l m n o p q r (EighteenOf18 z) = r z

----
data OneOf19 a b c d e f g h i j k l m n o p q r s
    = OneOf19 a | TwoOf19 b | ThreeOf19 c | FourOf19 d | FiveOf19 e
    | SixOf19 f | SevenOf19 g | EightOf19 h | NineOf19 i | TenOf19 j
    | ElevenOf19 k | TwelveOf19 l | ThirteenOf19 m | FourteenOf19 n
    | FifteenOf19 o | SixteenOf19 p | SeventeenOf19 q | EighteenOf19 r
    | NineteenOf19 s
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d,HTypeable e
         ,HTypeable f,HTypeable g,HTypeable h,HTypeable i,HTypeable j
         ,HTypeable k,HTypeable l,HTypeable m,HTypeable n,HTypeable o
         ,HTypeable p,HTypeable q,HTypeable r,HTypeable s)
    => HTypeable (OneOf19 a b c d e f g h i j k l m n o p q r s)
  where      toHType _ = Defined "OneOf19" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
         ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
         ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
         ,XmlContent p,XmlContent q,XmlContent r,XmlContent s)
    => XmlContent (OneOf19 a b c d e f g h i j k l m n o p q r s)
  where
    parseContents =
        (choice OneOf19 $ choice TwoOf19 $ choice ThreeOf19 $ choice FourOf19
        $ choice FiveOf19 $ choice SixOf19 $ choice SevenOf19
        $ choice EightOf19 $ choice NineOf19 $ choice TenOf19
        $ choice ElevenOf19 $ choice TwelveOf19 $ choice ThirteenOf19
        $ choice FourteenOf19 $ choice FifteenOf19 $ choice SixteenOf19
        $ choice SeventeenOf19 $ choice EighteenOf19 $ choice NineteenOf19
        $ fail "OneOf19")
    toContents (OneOf19 x) = toContents x
    toContents (TwoOf19 x) = toContents x
    toContents (ThreeOf19 x) = toContents x
    toContents (FourOf19 x) = toContents x
    toContents (FiveOf19 x) = toContents x
    toContents (SixOf19 x) = toContents x
    toContents (SevenOf19 x) = toContents x
    toContents (EightOf19 x) = toContents x
    toContents (NineOf19 x) = toContents x
    toContents (TenOf19 x) = toContents x
    toContents (ElevenOf19 x) = toContents x
    toContents (TwelveOf19 x) = toContents x
    toContents (ThirteenOf19 x) = toContents x
    toContents (FourteenOf19 x) = toContents x
    toContents (FifteenOf19 x) = toContents x
    toContents (SixteenOf19 x) = toContents x
    toContents (SeventeenOf19 x) = toContents x
    toContents (EighteenOf19 x) = toContents x
    toContents (NineteenOf19 x) = toContents x

foldOneOf19 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> (e->z) -> (f->z) -> 
               (g->z) -> (h->z) -> (i->z) -> (j->z) -> (k->z) -> (l->z) -> 
               (m->z) -> (n->z) -> (o->z) -> (p->z) -> (q->z) -> (r->z) -> 
               (s->z) -> 
               OneOf19 a b c d e f g h i j k l m n o p q r s
               -> z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (OneOf19 z) = a z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (TwoOf19 z) = b z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (ThreeOf19 z) = c z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (FourOf19 z) = d z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (FiveOf19 z) = e z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (SixOf19 z) = f z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (SevenOf19 z) = g z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (EightOf19 z) = h z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (NineOf19 z) = i z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (TenOf19 z) = j z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (ElevenOf19 z) = k z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (TwelveOf19 z) = l z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (ThirteenOf19 z) = m z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (FourteenOf19 z) = n z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (FifteenOf19 z) = o z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (SixteenOf19 z) = p z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (SeventeenOf19 z) = q z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (EighteenOf19 z) = r z
foldOneOf19 a b c d e f g h i j k l m n o p q r s (NineteenOf19 z) = s z

----
data OneOf20 a b c d e f g h i j k l m n o p q r s t
    = OneOf20 a | TwoOf20 b | ThreeOf20 c | FourOf20 d | FiveOf20 e
    | SixOf20 f | SevenOf20 g | EightOf20 h | NineOf20 i | TenOf20 j
    | ElevenOf20 k | TwelveOf20 l | ThirteenOf20 m | FourteenOf20 n
    | FifteenOf20 o | SixteenOf20 p | SeventeenOf20 q | EighteenOf20 r
    | NineteenOf20 s | TwentyOf20 t
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d,HTypeable e
         ,HTypeable f,HTypeable g,HTypeable h,HTypeable i,HTypeable j
         ,HTypeable k,HTypeable l,HTypeable m,HTypeable n,HTypeable o
         ,HTypeable p,HTypeable q,HTypeable r,HTypeable s,HTypeable t)
    => HTypeable (OneOf20 a b c d e f g h i j k l m n o p q r s t)
  where      toHType _ = Defined "OneOf20" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
         ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
         ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
         ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t)
    => XmlContent (OneOf20 a b c d e f g h i j k l m n o p q r s t)
  where
    parseContents =
        (choice OneOf20 $ choice TwoOf20 $ choice ThreeOf20 $ choice FourOf20
        $ choice FiveOf20 $ choice SixOf20 $ choice SevenOf20
        $ choice EightOf20 $ choice NineOf20 $ choice TenOf20
        $ choice ElevenOf20 $ choice TwelveOf20 $ choice ThirteenOf20
        $ choice FourteenOf20 $ choice FifteenOf20 $ choice SixteenOf20
        $ choice SeventeenOf20 $ choice EighteenOf20 $ choice NineteenOf20
        $ choice TwentyOf20
        $ fail "OneOf20")
    toContents (OneOf20 x) = toContents x
    toContents (TwoOf20 x) = toContents x
    toContents (ThreeOf20 x) = toContents x
    toContents (FourOf20 x) = toContents x
    toContents (FiveOf20 x) = toContents x
    toContents (SixOf20 x) = toContents x
    toContents (SevenOf20 x) = toContents x
    toContents (EightOf20 x) = toContents x
    toContents (NineOf20 x) = toContents x
    toContents (TenOf20 x) = toContents x
    toContents (ElevenOf20 x) = toContents x
    toContents (TwelveOf20 x) = toContents x
    toContents (ThirteenOf20 x) = toContents x
    toContents (FourteenOf20 x) = toContents x
    toContents (FifteenOf20 x) = toContents x
    toContents (SixteenOf20 x) = toContents x
    toContents (SeventeenOf20 x) = toContents x
    toContents (EighteenOf20 x) = toContents x
    toContents (NineteenOf20 x) = toContents x
    toContents (TwentyOf20 x) = toContents x

foldOneOf20 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> (e->z) -> (f->z) -> 
               (g->z) -> (h->z) -> (i->z) -> (j->z) -> (k->z) -> (l->z) -> 
               (m->z) -> (n->z) -> (o->z) -> (p->z) -> (q->z) -> (r->z) -> 
               (s->z) -> (t->z) -> 
               OneOf20 a b c d e f g h i j k l m n o p q r s t
               -> z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (OneOf20 z) = a z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (TwoOf20 z) = b z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (ThreeOf20 z) = c z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (FourOf20 z) = d z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (FiveOf20 z) = e z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (SixOf20 z) = f z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (SevenOf20 z) = g z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (EightOf20 z) = h z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (NineOf20 z) = i z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (TenOf20 z) = j z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (ElevenOf20 z) = k z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (TwelveOf20 z) = l z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (ThirteenOf20 z) = m z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (FourteenOf20 z) = n z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (FifteenOf20 z) = o z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (SixteenOf20 z) = p z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (SeventeenOf20 z) = q z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (EighteenOf20 z) = r z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (NineteenOf20 z) = s z
foldOneOf20 a b c d e f g h i j k l m n o p q r s
            t (TwentyOf20 z) = t z

----
