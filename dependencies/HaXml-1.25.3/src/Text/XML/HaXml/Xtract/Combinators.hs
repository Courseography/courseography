-- | This is a new set of XML combinators for Xtract, not standard,
--   but based on the standard set in "Text.Xml.Haxml.Combinators".
--   The main difference is that the Content Filter type becomes a
--   Double Filter.  A Double Filter always takes the whole document
--   as an extra argument, so you can start to traverse it again from
--   the root, when at any inner location within the document tree.
--
--   The new combinator definitions are derived from the old ones.
--   The same names have the equivalent meaning - use module qualification
--   on imports to distinguish between CFilter and DFilter variations.

module Text.XML.HaXml.Xtract.Combinators where

import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators (CFilter)
import qualified Text.XML.HaXml.Combinators as C


-- | double content filter - takes document root + local subtree.
type DFilter i = Content i -> Content i -> [Content i]

-- | lift an ordinary content filter to a double filter.
local,global :: CFilter i -> DFilter i
local  f = \_xml  sub-> f sub
global f = \ xml _sub-> f xml

-- | drop a double filter to an ordinary content filter.
--   (permitting interior access to document root)
dfilter :: DFilter i -> CFilter i
dfilter f = \xml-> f xml xml

-- | drop a double filter to an ordinary content filter.
--   (Where interior access to the document root is not needed, the
--    retaining pointer to the outer element can be pruned away.
--   'cfilter' is more space-efficient than 'dfilter' in this situation.)
cfilter :: DFilter i -> CFilter i
cfilter f = \xml -> f undefined xml
--cfilter f = \xml-> flip f xml
--                          (case xml of
--                             CElem (Elem n as cs) i -> CElem (Elem n [] []) i
--                             _ -> xml)

-- | lift a CFilter combinator to a DFilter combinator
liftLocal, liftGlobal :: (CFilter i->CFilter i) -> (DFilter i->DFilter i)
liftLocal  ff = \df-> \xml  sub-> (ff (df xml)) sub
liftGlobal ff = \df-> \xml _sub-> (ff (df xml)) xml

-- | lifted composition over double filters.
o :: DFilter i -> DFilter i -> DFilter i
g `o` f = \xml-> concatMap (g xml) . (f xml)

-- | lifted choice.
(|>|) :: (a->b->[c]) -> (a->b->[c]) -> (a->b->[c])
f |>| g = \xml sub-> let first = f xml sub in
                     if null first then g xml sub else first

-- | lifted union.
union :: (a->b->[c]) -> (a->b->[c]) -> (a->b->[c])
union = lift (++)
  where
    lift f g h = \x y-> f (g x y) (h x y)

-- | lifted predicates.
with, without :: DFilter i -> DFilter i -> DFilter i
f `with` g    = \xml-> filter (not.null.g xml) . f xml
f `without` g = \xml-> filter     (null.g xml) . f xml

-- | lifted unit and zero.
keep, none :: DFilter i
keep = \_xml  sub-> [sub]	-- local C.keep
none = \_xml _sub-> []	-- local C.none

children, elm, txt :: DFilter i
children = local C.children
elm      = local C.elm
txt      = local C.txt

applypred :: CFilter i -> DFilter i -> CFilter i
applypred f p = \xml-> (const f `with` p) xml xml

iffind :: String -> (String -> DFilter i) -> DFilter i -> DFilter i
iffind  key  yes no xml c@(CElem (Elem _ as _) _) =
  case (lookup (N key) as) of
    Nothing -> no xml c
    (Just v@(AttValue _)) -> yes (show v) xml c
iffind _key _yes no xml other = no xml other

ifTxt :: (String->DFilter i) -> DFilter i -> DFilter i
ifTxt  yes _no xml c@(CString _ s _) = yes s xml c
ifTxt _yes  no xml c                 = no xml c

cat :: [a->b->[c]] -> (a->b->[c])
cat fs = \xml sub-> concat [ f xml sub | f <- fs ]

(/>) :: DFilter i -> DFilter i -> DFilter i
f /> g = g `o` children `o` f

(</) :: DFilter i -> DFilter i -> DFilter i
f </ g = f `with` (g `o` children)

deep, deepest, multi :: DFilter i -> DFilter i
deep f    = f |>| (deep f `o` children)
deepest f = (deepest f `o` children) |>| f
multi f   = f `union` (multi f `o` children)
