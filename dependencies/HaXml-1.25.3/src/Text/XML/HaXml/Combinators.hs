--------------------------------------------
-- | This module defines the notion of filters and filter combinators
--   for processing XML documents.
--
--   These XML transformation combinators are described in the paper
--   ``Haskell and XML: Generic Combinators or Type-Based Translation?''
--   Malcolm Wallace and Colin Runciman, Proceedings ICFP'99.
--------------------------------------------
module Text.XML.HaXml.Combinators
  (-- * The content filter type.
    CFilter

   -- * Simple filters.
   -- ** Selection filters.
   -- $selection
  , keep, none, children, childrenBy, position

   -- ** Predicate filters.
   -- $pred
  , elm, txt, tag, attr, attrval, tagWith

   -- ** Search filters.
  , find, iffind, ifTxt

   -- * Filter combinators
   -- ** Basic combinators.
  , o, union, cat, andThen
  , (|>|), with, without
  , (/>), (</), et
  , path
   -- ** Recursive search.
   -- $recursive
  , deep, deepest, multi
   -- ** Interior editing.
  , when, guards, chip, inplace, recursivelyInPlace, foldXml
   -- ** Constructive filters.
   -- $constructive
  , mkElem, mkElemAttr, literal, cdata, replaceTag, replaceAttrs, addAttribute

   -- * C-like conditionals.
   -- $cond
  , ThenElse(..), (?>)

   -- * Filters with labelled results.
  , LabelFilter
   -- ** Using and combining labelled filters.
  , oo, x
   -- ** Some label-generating filters.
  , numbered, interspersed, tagged, attributed, textlabelled, extracted

  ) where


import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces
import Data.Maybe (fromMaybe)

infixl 6 `with`, `without`
infixr 5 `o`, `oo`, `union`, `andThen`		-- , `orelse`
infixl 5 />, </, |>|
infixr 4 `when`, `guards`
infixr 3 ?>, :>



-- THE CONTENT FILTER TYPE

-- | All document transformations are /content filters/.
--   A filter takes a single XML 'Content' value and returns a sequence
--   of 'Content' values, possibly empty.
type CFilter i  = Content i -> [Content i]



-- BASIC SELECTION FILTERS
-- $selection
-- In the algebra of combinators, @none@ is the zero, and @keep@ the identity.
-- (They have a more general type than just CFilter.)
keep :: a->[a]
keep = \x->[x]
none :: a->[b]
none = \x->[]

-- | Throw away current node, keep just the (unprocessed) children.
children :: CFilter i
children (CElem (Elem _ _ cs) _) = cs
children _ = []

-- | Select the @n@'th positional result of a filter.
position :: Int -> CFilter i -> CFilter i
position n f = (\cs-> [cs!!n]) . f



-- BASIC PREDICATE FILTERS
-- $pred
-- These filters either keep or throw away some content based on
-- a simple test.  For instance, @elm@ keeps only a tagged element,
-- @txt@ keeps only non-element text, @tag@ keeps only an element
-- with the named tag, @attr@ keeps only an element with the named
-- attribute, @attrval@ keeps only an element with the given
-- attribute value, @tagWith@ keeps only an element whose tag name
-- satisfies the given predicate.

elm, txt   :: CFilter i
tag        :: String -> CFilter i
attr       :: String -> CFilter i
attrval    :: Attribute -> CFilter i
tagWith    :: (String->Bool) -> CFilter i

elm x@(CElem _ _) = [x]
elm _             = []

txt x@(CString _ _ _) = [x]
txt x@(CRef _ _)      = [x]
txt _                 = []

tag t x@(CElem (Elem n _ _) _) | t==printableName n  = [x]
tag _ _  = []

tagWith p x@(CElem (Elem n _ _) _) | p (printableName n)  = [x]
tagWith _ _  = []

attr n x@(CElem (Elem _ as _) _) | n `elem` (map (printableName.fst) as)  = [x]
attr _ _  = []

attrval av x@(CElem (Elem _ as _) _) | av `elem` as  = [x]
attrval _  _  = []



-- SEARCH FILTERS

-- | For a mandatory attribute field, @find key cont@ looks up the value of
--   the attribute name @key@, and applies the continuation @cont@ to
--   the value.
find :: String -> (String->CFilter i) -> CFilter i
find key cont c@(CElem (Elem _ as _) _) = cont (show (lookfor (N key) as)) c
  where lookfor x = fromMaybe (error ("missing attribute: "++key)) . lookup x
-- 'lookfor' has the more general type :: (Eq a,Show a) => a -> [(a,b)] -> b

-- | When an attribute field may be absent, use @iffind key yes no@ to lookup
--   its value.  If the attribute is absent, it acts as the @no@ filter,
--   otherwise it applies the @yes@ filter.
iffind :: String -> (String->CFilter i) -> CFilter i -> CFilter i
iffind  key  yes no c@(CElem (Elem _ as _) _) =
  case (lookup (N key) as) of
    Nothing               -> no c
    (Just v@(AttValue _)) -> yes (show v) c
iffind _key _yes no other = no other

-- | @ifTxt yes no@ processes any textual content with the @yes@ filter,
--   but otherwise is the same as the @no@ filter.
ifTxt :: (String->CFilter i) -> CFilter i -> CFilter i
ifTxt  yes _no c@(CString _ s _) = yes s c
ifTxt _yes  no c                 = no c



-- C-LIKE CONDITIONALS
--
-- $cond
-- These definitions provide C-like conditionals, lifted to the filter level.
--
-- The @(cond ? yes : no)@ style in C becomes @(cond ?> yes :> no)@ in Haskell.

-- | Conjoin the two branches of a conditional.
data ThenElse a = a :> a

-- | Select between the two branches of a joined conditional.
(?>) :: (a->[b]) -> ThenElse (a->[b]) -> (a->[b])
p ?> (f :> g) = \c-> if (not.null.p) c then f c else g c



-- FILTER COMBINATORS


-- | Sequential (/Irish/,/backwards/) composition
o :: CFilter i -> CFilter i -> CFilter i
f `o` g = concatMap f . g

-- | Binary parallel composition.  Each filter uses a copy of the input,
-- rather than one filter using the result of the other.
--   (Has a more general type than just CFilter.)
union :: (a->[b]) -> (a->[b]) -> (a->[b])
union = lift (++)		-- in Haskell 98:   union = lift List.union
  where
    lift :: (a->b->d) -> (c->a) -> (c->b) -> c -> d
    lift f g h = \x-> f (g x) (h x)

-- | Glue a list of filters together.  (A list version of union;
--   also has a more general type than just CFilter.)
cat :: [a->[b]] -> (a->[b])
--   Specification: cat fs = \e-> concat [ f e | f <- fs ]
--   more efficient implementation below:
cat [] = const []
cat fs = foldr1 union fs

-- | A special form of filter composition where the second filter
--   works over the same data as the first, but also uses the
--   first's result.
andThen :: (a->c) -> (c->a->b) -> (a->b)
andThen f g = \x-> g (f x) x			-- lift g f id

-- | Process children using specified filters.
childrenBy :: CFilter i -> CFilter i 
childrenBy f = f `o` children

-- | Directional choice:
--   in @f |>| g@ give g-productions only if no f-productions
(|>|) :: (a->[b]) -> (a->[b]) -> (a->[b])
f |>| g = \x-> let fx = f x in if null fx then g x else fx
--      f |>| g  =  f ?> f :> g

-- | Pruning: in @f `with` g@,
--   keep only those f-productions which have at least one g-production
with :: CFilter i -> CFilter i -> CFilter i
f `with` g = filter (not.null.g) . f

-- | Pruning: in @f `without` g@,
--   keep only those f-productions which have no g-productions
without :: CFilter i -> CFilter i -> CFilter i
f `without` g = filter (null.g) . f

-- | Pronounced /slash/, @f \/> g@ means g inside f
(/>) :: CFilter i -> CFilter i -> CFilter i
f /> g = g `o` children `o` f

-- | Pronounced /outside/, @f \<\/ g@ means f containing g
(</) :: CFilter i -> CFilter i -> CFilter i
f </ g = f `with` (g `o` children)

-- | Join an element-matching filter with a text-only filter
et :: (String->CFilter i) -> CFilter i -> CFilter i
et f g = (f `oo` tagged elm)
            |>|
         (g `o` txt)

-- | Express a list of filters like an XPath query, e.g.
--   @path [children, tag \"name1\", attr \"attr1\", children, tag \"name2\"]@
--   is like the XPath query @\/name1[\@attr1]\/name2@.
path :: [CFilter i] -> CFilter i
path fs = foldr (flip (o)) keep fs


-- RECURSIVE SEARCH
-- $recursive
-- Recursive search has three variants: @deep@ does a breadth-first
-- search of the tree, @deepest@ does a depth-first search, @multi@ returns
-- content at all tree-levels, even those strictly contained within results
-- that have already been returned.
deep, deepest, multi :: CFilter i -> CFilter i
deep f     = f |>| (deep f `o` children)
deepest f  = (deepest f `o` children) |>| f
multi f    = f `union` (multi f `o` children)

-- | Interior editing:
--   @f `when` g@ applies @f@ only when the predicate @g@ succeeds,
--   otherwise the content is unchanged.
when   :: CFilter i -> CFilter i -> CFilter i
-- | Interior editing:
--   @g `guards` f@ applies @f@ only when the predicate @g@ succeeds,
--   otherwise the content is discarded.
guards :: CFilter i -> CFilter i -> CFilter i
f `when` g       = g ?> f :> keep
g `guards` f     = g ?> f :> none	-- = f `o` (keep `with` g)

-- | Process CHildren In Place.  The filter is applied to any children
--   of an element content, and the element rebuilt around the results.
chip :: CFilter i -> CFilter i
chip  f (CElem (Elem n as cs) i) = [ CElem (Elem n as (concatMap f cs)) i ]
chip _f c = [c]
-- chip f = inplace (f `o` children)

-- | Process an element In Place.  The filter is applied to the element
--   itself, and then the original element rebuilt around the results.
inplace :: CFilter i -> CFilter i
inplace  f c@(CElem (Elem name as _) i) = [ CElem (Elem name as (f c)) i ]
inplace _f c = [c]

-- | Recursively process an element in place.  That is, the filter is
--   applied to the element itself, then recursively to the results of the
--   filter, all the way to the bottom, then the original element rebuilt
--   around the final results.
recursivelyInPlace :: CFilter i -> CFilter i
recursivelyInPlace f = inplace (recursivelyInPlace f `o` f)


-- | Recursive application of filters: a fold-like operator.  Defined
--   as @f `o` chip (foldXml f)@.
foldXml :: CFilter i -> CFilter i
foldXml f = f `o` chip (foldXml f)




-- CONSTRUCTIVE CONTENT FILTERS
--
-- $constructive
-- The constructive filters are primitive filters for building new elements,
-- or editing existing elements.

-- | Build an element with the given tag name - its content is the results
--   of the given list of filters.
mkElem :: String -> [CFilter i] -> CFilter i
mkElem h cfs = \t-> [ CElem (Elem (N h) [] (cat cfs t)) undefined ]

-- | Build an element with the given name, attributes, and content.
mkElemAttr :: String -> [(String,CFilter i)] -> [CFilter i] -> CFilter i
mkElemAttr h as cfs = \t-> [ CElem (Elem (N h) (map (attr t) as) (cat cfs t))
                                   undefined ]
  where attr t (n,vf) =
            let v = concat [ s | (CString _ s _) <- (deep txt `o` vf) t ]
            in  (N n, AttValue [Left v])

-- | Build some textual content.
literal :: String -> CFilter i
literal s = const [CString False s undefined]

-- | Build some CDATA content.
cdata :: String -> CFilter i
cdata s = const [CString True s undefined]

-- | Rename an element tag (leaving attributes in place).
replaceTag :: String -> CFilter i
replaceTag n (CElem (Elem _ as cs) i) = [CElem (Elem (N n) as cs) i]
replaceTag _ _ = []

-- | Replace the attributes of an element (leaving tag the same).
replaceAttrs :: [(String,String)] -> CFilter i
replaceAttrs as (CElem (Elem n _ cs) i) = [CElem (Elem n as' cs) i]
    where as' = map (\(n,v)-> (N n, AttValue [Left v])) as
replaceAttrs _  _ = []

-- | Add the desired attribute name and value to the topmost element,
--   without changing the element in any other way.
addAttribute :: String -> String -> CFilter a
addAttribute name val (CElem (Elem n   as   cs) i) =
                      [CElem (Elem n (a:as) cs) i]
  where a = (N name, AttValue [Left val])
addAttribute _ _ _ = []



-- LABELLING
-- $labelling
-- LabelFilters are a way of annotating the results of a filter operation
-- with some arbitrary values drawn from the tree values.  Typically, the
-- annotations are then consumed by a label-processing filter (of
-- type @a -> CFilter@).  This is useful way of passing information between
-- sections of the tree as you process it.  An example may help to explain.
--
-- Let's say we want to add an attribute to every node of the tree,
-- containing a textual representation of its path from the root,
-- e.g. "/foo/bar/quux".  Where there are multiple identically-tagged elements
-- under the same parent node of the original tree, we expect them to have
-- a distinguishing attribute called "name".
--
-- Step one.  Given the path prefix to this node, how do we add the "xpath"
-- attribute?
--
-- > annotateOne :: String -> CFilter a
-- > annotateOne prefix =
-- >    (f `oo` ((tagged `x` attributed "name") (attr "name")))
-- >    |>|
-- >    (g `oo` (tagged keep))
-- >  where
-- >    f (tag,att) = addAttribute "xpath" (prefix++"/"++tag++"["++att++"]")
-- >    g  tag      = addAttribute "xpath" (prefix++"/"++tag)@
--
-- First, the @attr "name"@ filter distinguishes whether this node contains
-- the attribute, hence choosing whether the left or right branch of the
-- @|>|@ is taken.  If the attribute is /not/ present, then the LabelFilter
-- @tagged keep@ selects the current node, and annotates it with the
-- tagname of the element.  The @oo@ applies the label-consuming function @g@
-- to the result, and this injects the "xpath" attribute by suffixing
-- the tagname to the known path prefix.
--
-- If the "name" attribute /is/ present, then there are /two/ labelling filters
-- applied to the current node, annotating it with the pair of its tag
-- and the value of the attribute "name".  The label-consuming function @f@ is
-- applied to the pair with @oo@, to inject the "xpath" attribute with a more
-- complex representation of its path.
--
-- Step two.  Recursively apply the annotation throughout the tree.
--
-- > labelAllPaths :: CFilter a
-- > labelAllPaths = allPaths `o` initialise
-- >   where
-- >     initialise = annotateOne "/"
-- > 
-- >     allPaths :: CFilter a
-- >     allPaths = inplace ( allPaths
-- >                          `o`
-- >                          (\prefix-> annotateOne prefix `o` children)
-- >                          `oo`
-- >                          (attributed "xpath" keep)
-- >                        )
--
-- In order to apply @annotateOne@ to any node, we need to know the path
-- prefix thus far into the tree.  So, we read the "xpath" attribute from
-- the current node (assumed to have already been processed) as a
-- LabelFilter, then consume the label by passing it to @annotateOne@ on
-- the children of the current node.  Using @inplace@ rebuilds the processed
-- children into the current node, after recursively dealing with their
-- children.



-- | A LabelFilter is like a CFilter except that it pairs up a polymorphic
--   value (label) with each of its results.
type LabelFilter i a = Content i -> [(a,Content i)]

-- | Compose a label-processing filter with a label-generating filter.
oo :: (a->CFilter i) -> LabelFilter i a -> CFilter i
f `oo` g = concatMap (uncurry f) . g

{-
-- | Process the information labels (very nearly monadic bind).
oo :: (b -> CFilter b c) -> CFilter a b -> CFilter a c
f `oo` g = concatMap info . g
    where info c@(CElem _ i)     = f i c
          info c@(CString _ _ i) = f i c
          info c@(CRef _ i)      = f i c
          info c                 = [c]
-}

-- | Combine labels.  Think of this as a pair-wise zip on labels.
--   e.g. @(numbered `x` tagged)@
x :: (CFilter i->LabelFilter i a) -> (CFilter i->LabelFilter i b) ->
       (CFilter i->LabelFilter i (a,b))
f `x` g = \cf c-> let gs = map fst (g cf c)
                      fs = map fst (f cf c)
                  in zip (zip fs gs) (cf c)


-- Some basic label-generating filters.

-- | Number the results from 1 upwards.
numbered :: CFilter i -> LabelFilter i Int
numbered f = zip [1..] . f

-- | In @interspersed a f b@, label each result of @f@ with the string @a@,
--   except for the last one which is labelled with the string @b@.
interspersed :: String -> CFilter i -> String -> LabelFilter i String
interspersed a f b =
  (\xs-> zip (replicate (len xs) a ++ [b]) xs) . f
  where
  len [] = 0
  len xs = length xs - 1

-- | Label each element in the result with its tag name.  Non-element
--   results get an empty string label.
tagged :: CFilter i -> LabelFilter i String
tagged f = extracted name f
  where name (CElem (Elem n _ _) _) = printableName n
        name _                      = ""

-- | Label each element in the result with the value of the named attribute.
--   Elements without the attribute, and non-element results, get an
--   empty string label.
attributed :: String -> CFilter i -> LabelFilter i String
attributed key f = extracted att f
  where att (CElem (Elem _ as _) _) =
            case (lookup (N key) as) of
              Nothing  -> ""
              (Just v@(AttValue _)) -> show v
        att _ = ""

-- | Label each textual part of the result with its text.  Element
--   results get an empty string label.
textlabelled :: CFilter i -> LabelFilter i (Maybe String)
textlabelled f = extracted text f
  where text (CString _ s _) = Just s
        text _               = Nothing

-- | Label each content with some information extracted from itself.
extracted :: (Content i->a) -> CFilter i -> LabelFilter i a
extracted proj f = concatMap (\c->[(proj c, c)]) . f
                                                                                


{-
-- MISC

-- | I haven't yet remembered \/ worked out what this does.
combine :: (Read a,Show a) => ([a]->a) -> LabelFilter String -> CFilter
combine f lf = \c-> [ CString False (show (f [ read l | (l,_) <- lf c ])) ]
-}


{- OLD STUFF - OBSOLETE
-- Keep an element by its numbered position (starting at 1).
position :: Int -> [Content] -> [Content]
position n | n>0  = (:[]) . (!!(n-1))
           | otherwise = const []

-- Chop and remove the root portions of trees to depth n.
layer :: Int -> [Content] -> [Content]
layer n = apply n (concatMap lay)
  where lay (CElem (Elem _ _ cs)) = cs
        lay _ = []
        apply 0 f xs = xs
        apply n f xs = apply (n-1) f (f xs)

combine :: (Read a, Show a) => ([a]->a) -> [Content] -> [Content]
combine f = \cs-> [ CString False (show (f [ read s | CString _ s <- cs ])) ]
-}
