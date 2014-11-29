{-# OPTIONS_HADDOCK hide #-}
module Text.Blaze.Svg.Internal where

import Control.Monad.State
import Data.Monoid (mappend, mempty)

import Text.Blaze

-- | Type to represent an SVG document fragment.
type Svg = Markup

-- | Type to accumulate an SVG path.
type Path = State AttributeValue ()

-- | Construct SVG path values using path instruction combinators.
-- See simple example below of how you can use @mkPath@ to
-- specify a path using the path instruction combinators
-- that are included as part of the same module.
--
-- More information available at: <http://www.w3.org/TR/SVG/paths.html>
--
-- > import Text.Blaze.Svg11 ((!), mkPath, l, m)
-- > import qualified Text.Blaze.Svg11 as S
-- > import qualified Text.Blaze.Svg11.Attributes as A
-- >
-- > svgDoc :: S.Svg
-- > svgDoc = S.docTypeSvg ! A.version "1.1" ! A.width "150" ! A.height "100" $ do
-- >  S.path ! A.d makeSimplePath
-- >
-- > makeSimplePath :: S.AttributeValue
-- > makeSimplePath =  mkPath do
-- >   l 2 3
-- >   m 4 5
mkPath :: Path -> AttributeValue
mkPath path = snd $ runState path mempty

appendToPath :: [String] -> Path
appendToPath  = modify . flip mappend . toValue . join

-- | Moveto
m :: Show a => a -> a -> Path
m x y = appendToPath
  [ "M "
  , show x, ",", show y
  , " "
  ]

-- | Moveto (relative)
mr :: Show a => a -> a -> Path
mr dx dy = appendToPath
  [ "m "
  , show dx, ",", show dy
  , " "
  ]

-- | ClosePath
z :: Path
z = modify (`mappend` toValue "Z")

-- | Lineto
l :: Show a => a -> a -> Path
l x y = appendToPath
  [ "L "
  , show x, ",", show y
  , " "
  ]

-- | Lineto (relative)
lr :: Show a => a -> a -> Path
lr dx dy = appendToPath
  [ "l "
  , show dx, ",", show dy
  , " "
  ]

-- | Horizontal lineto
h :: Show a => a -> Path
h x = appendToPath
  [ "H "
  , show x
  , " "
  ]

-- | Horizontal lineto (relative)
hr :: Show a => a -> Path
hr dx = appendToPath
  [ "h "
  , show dx
  , " "
  ]


-- | Vertical lineto
v :: Show a => a -> Path
v y = appendToPath
  [ "V "
  , show y
  , " "
  ]

-- | Vertical lineto (relative)
vr :: Show a => a -> Path
vr dy = appendToPath
  [ "v "
  , show dy
  , " "
  ]

-- | Cubic Bezier curve
c :: Show a => a -> a -> a -> a -> a -> a -> Path
c c1x c1y c2x c2y x y = appendToPath
  [ "C "
  , show c1x, ",", show c1y
  , " "
  , show c2x, ",", show c2y
  , " "
  , show x, " ", show y
  ]

-- | Cubic Bezier curve (relative)
cr :: Show a => a -> a -> a -> a -> a -> a -> Path
cr dc1x dc1y dc2x dc2y dx dy = appendToPath
  [ "c "
  , show dc1x, ",", show dc1y
  , " "
  , show dc2x, ",", show dc2y
  , " "
  , show dx, " ", show dy
  ]

-- | Smooth Cubic Bezier curve
s :: Show a => a -> a -> a -> a -> Path
s c2x c2y x y = appendToPath
  [ "S "
  , show c2x, ",", show c2y
  , " "
  , show x, ",", show y
  , " "
  ]

-- | Smooth Cubic Bezier curve (relative)
sr :: Show a => a -> a -> a -> a -> Path
sr dc2x dc2y dx dy = appendToPath
  [ "s "
  , show dc2x, ",", show dc2y
  , " "
  , show dx, ",", show dy
  , " "
  ]

-- | Quadratic Bezier curve
q :: Show a => a -> a -> a -> a -> Path
q cx cy x y = appendToPath
  [ "Q "
  , show cx, ",", show cy
  , " "
  , show x, ",", show y
  , " "
  ]

-- | Quadratic Bezier curve (relative)
qr :: Show a => a -> a -> a -> a  -> Path
qr dcx dcy dx dy = appendToPath
  [ "q "
  , show dcx, ",", show dcy
  , " "
  , show dx, ",", show dy
  , " "
  ]

-- | Smooth Quadratic Bezier curve
t  :: Show a => a -> a -> Path
t x y = appendToPath
  [ "T "
  , " "
  , show x, ",", show y
  , " "
  ]

-- | Smooth Quadratic Bezier curve (relative)
tr :: Show a => a -> a -> Path
tr x y = appendToPath
  [ "t "
  , " "
  , show x, ",", show y
  , " "
  ]

-- | Specifies a translation by @x@ and @y@
translate :: Show a => a -> a -> AttributeValue
translate x y = toValue . join $
  [ "translate("
  , show x, " ", show y
  , ")"
  ]

-- | Specifies a scale operation by @x@ and @y@
scale :: Show a => a -> a -> AttributeValue
scale x y = toValue . join $
  [ "scale("
  , show x, " ", show y
  , ")"
  ]

-- | Specifies a rotation by @rotate-angle@ degrees
rotate :: Show a => a -> AttributeValue
rotate rotateAngle = toValue . join $
  [ "rotate("
  , show rotateAngle
  , ")"
  ]

-- | Specifies a rotation by @rotate-angle@ degrees about the given time @rx,ry@
rotateAround :: Show a => a -> a -> a -> AttributeValue
rotateAround rotateAngle rx ry = toValue . join $
  [ "rotate("
  , show rotateAngle, ","
  , show rx, ",", show ry
  , ")"
  ]

-- | Skew tansformation along x-axis
skewX :: Show a => a -> AttributeValue
skewX skewAngle = toValue . join $
  [ "skewX("
  , show skewAngle
  , ")"
  ]

-- | Skew tansformation along y-axis
skewY :: Show a => a -> AttributeValue
skewY skewAngle = toValue . join $
  [ "skewY("
  , show skewAngle
  , ")"
  ]

-- | Specifies a transform in the form of a transformation matrix
matrix :: Show a => a -> a -> a -> a -> a -> a -> AttributeValue
matrix a b c_ d e f =  toValue . join $
  [  "matrix("
  ,  show a, ","
  ,  show b, ","
  ,  show c_, ","
  ,  show d, ","
  ,  show e, ","
  ,  show f
  , ")"
  ]
