{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Predefined and custom colors. Essentially equivalent to the
-- "Graphics.Gloss.Data.Color", but lifted to Accelerate types.
--
module Graphics.Gloss.Data.Accelerate.Color (

  -- ** Color data type
  Color,
  makeColor,
  makeColor8,
  rawColor,
  rgbaOfColor,
  packRGBA, packABGR,

  -- ** Color functions
  mixColors,
  addColors,
  dim, bright,
  light, dark,
  opaque,

  -- ** Pre-defined colors
  greyN, black, white,

  -- *** Primary
  red, green, blue,

  -- *** Secondary
  yellow, cyan, magenta,

  -- *** Tertiary
  rose, violet, azure, aquamarine, chartreuse, orange,

) where

import Prelude                                  as P
import Data.Bits
import Data.Typeable
import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple              ( Tuple(..), TupleIdx(..), IsTuple(..), )
import Data.Array.Accelerate.Array.Sugar        ( Elt(..), EltRepr, EltRepr' )


-- | An abstract color value.
--
-- We keep the type abstract so we can be sure that the components are in the
-- required range. To make a custom color use 'makeColor'.
--
type Color = RGBA Float


-- | An RGBA colour value to hold the color components. All components lie in
-- the range [0..1).
--
-- We need to parameterise by a type so that we can have both Exp (RGBA a) and
-- RGBA (Exp a).
--
data RGBA a = RGBA a a a a
  deriving (Show, Eq, Typeable)


instance Num a => Num (RGBA a) where
  (+) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 + r2) (g1 + g2) (b1 + b2) 1

  (-) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 - r2) (g1 - g2) (b1 - b2) 1

  (*) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 * r2) (g1 * g2) (b1 * b2) 1

  abs (RGBA r1 g1 b1 _)
        = RGBA (abs r1) (abs g1) (abs b1) 1

  signum (RGBA r1 g1 b1 _)
        = RGBA (signum r1) (signum g1) (signum b1) 1

  fromInteger i
        = let f = fromInteger i
          in  RGBA f f f 1


-- Represent colours in Accelerate as a 4-tuple
--
type instance EltRepr  (RGBA a) = EltRepr (a, a, a, a)
type instance EltRepr' (RGBA a) = EltRepr (a, a, a, a)

instance Elt a => Elt (RGBA a) where
  eltType (_ :: RGBA a)         = eltType (undefined :: (a,a,a,a))
  toElt c                       = let (r,g,b,a) = toElt c in RGBA r g b a
  fromElt (RGBA r g b a)        = fromElt (r,g,b,a)

  eltType' (_ :: RGBA a)        = eltType' (undefined :: (a,a,a,a))
  toElt' c                      = let (r,g,b,a) = toElt' c in RGBA r g b a
  fromElt' (RGBA r g b a)       = fromElt' (r,g,b,a)

instance IsTuple (RGBA a) where
  type TupleRepr (RGBA a) = (((((),a), a), a), a)
  fromTuple (RGBA r g b a)      = (((((), r), g), b), a)
  toTuple (((((),r),g),b),a)    = RGBA r g b a

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (RGBA a) where
  type Plain (RGBA a) = RGBA (Plain a)
  lift (RGBA r g b a)   = Exp . Tuple $ NilTup `SnocTup` lift r `SnocTup` lift g
                                               `SnocTup` lift b `SnocTup` lift a

instance Elt a => Unlift Exp (RGBA (Exp a)) where
  unlift c      = let r = Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` c
                      g = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` c
                      b = Exp $ SuccTupIdx ZeroTupIdx `Prj` c
                      a = Exp $ ZeroTupIdx `Prj` c
                  in RGBA r g b a


-- | Make a custom color. All components are clamped to the range  [0..1].
--
makeColor
    :: Exp Float        -- ^ Red component.
    -> Exp Float        -- ^ Green component.
    -> Exp Float        -- ^ Blue component.
    -> Exp Float        -- ^ Alpha component.
    -> Exp Color
makeColor r g b a
  = clampColor
  $ rawColor r g b a


-- | Make a custom color.
--   You promise that all components are clamped to the range [0..1]
--
rawColor :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Color
rawColor r g b a = lift (RGBA r g b a)


-- | Make a custom color from 8-bit values.
--
makeColor8
    :: Exp Word8        -- ^ Red component.
    -> Exp Word8        -- ^ Green component.
    -> Exp Word8        -- ^ Blue component.
    -> Exp Word8        -- ^ Alpha component.
    -> Exp Color
makeColor8 r g b a
  = clampColor
  $ rawColor (A.fromIntegral r / 255)
             (A.fromIntegral g / 255)
             (A.fromIntegral b / 255)
             (A.fromIntegral a / 255)


-- | Take the RGBA components of a color.
rgbaOfColor :: Exp Color -> (Exp Float, Exp Float, Exp Float, Exp Float)
rgbaOfColor c
  = let (RGBA r g b a) = unlift c
    in  (r, g, b, a)


-- Internal
-- --------

-- | Clamp components of a color into the required range.
--
clampColor :: Exp Color -> Exp Color
clampColor cc
  = let (r, g, b, a)    = rgbaOfColor cc
    in  lift $ RGBA (min 1 r) (min 1 g) (min 1 b) (min 1 a)


-- | Normalise a color to the value of its largest RGB component.
--
normaliseColor :: Exp Color -> Exp Color
normaliseColor cc
  = let (r, g, b, a)    = rgbaOfColor cc
        m               = P.maximum [r, g, b]
    in  lift $ RGBA (r / m) (g / m) (b / m) a


-- | Convert a color into a packed RGBA value.
--
packRGBA :: Exp Color -> Exp Word32
packRGBA c
  = let (r, g, b, a)    = rgbaOfColor c
    in  word32OfFloat r `A.shiftL` 24
    .|. word32OfFloat g `A.shiftL` 16
    .|. word32OfFloat b `A.shiftL` 8
    .|. word32OfFloat a

-- | Convert a colour into a packed BGRA value.
--
-- This is necessary as OpenGL reads pixel data as ABGR, rather than RGBA.
--
packABGR :: Exp Color -> Exp Word32
packABGR c
  = let (r, g, b, a)    = rgbaOfColor c
    in  word32OfFloat a `A.shiftL` 24
    .|. word32OfFloat b `A.shiftL` 16
    .|. word32OfFloat g `A.shiftL` 8
    .|. word32OfFloat r

word32OfFloat :: Exp Float -> Exp Word32
word32OfFloat f = A.truncate (f * 255)


-- Color functions ------------------------------------------------------------

-- | Mix two colors with the given ratios.
--
mixColors
    :: Exp Float        -- ^ Ratio of first color.
    -> Exp Float        -- ^ Ratio of second color.
    -> Exp Color        -- ^ First color.
    -> Exp Color        -- ^ Second color.
    -> Exp Color        -- ^ Resulting color.

mixColors ratio1 ratio2 c1 c2
  = let RGBA r1 g1 b1 a1        = unlift c1
        RGBA r2 g2 b2 a2        = unlift c2

        total   = ratio1 + ratio2
        m1      = ratio1 / total
        m2      = ratio2 / total
   in
   rawColor (m1 * r1 + m2 * r2)
            (m1 * g1 + m2 * g2)
            (m1 * b1 + m2 * b2)
            (m1 * a1 + m2 * a2)


-- | Add RGB components of a color component-wise, then normalise them to the
--   highest resulting one. The alpha components are averaged.
--
addColors :: Exp Color -> Exp Color -> Exp Color
addColors c1 c2
  = let RGBA r1 g1 b1 a1        = unlift c1
        RGBA r2 g2 b2 a2        = unlift c2
    in
    normaliseColor $ rawColor (r1 + r2) (g1 + g2) (b1 + b2) ((a1 + a2) / 2)

-- | Make a dimmer version of a color, scaling towards black.
dim :: Exp Color -> Exp Color
dim c
  = let RGBA r g b a            = unlift c
    in  rawColor (r / 1.2) (g / 1.2) (b / 1.2) a

-- | Make a brighter version of a color, scaling towards white.
bright :: Exp Color -> Exp Color
bright c
  = let RGBA r g b a            = unlift c
    in clampColor $ rawColor (r * 1.2) (g * 1.2) (b * 1.2) a

-- | Lighten a color, adding white.
light :: Exp Color -> Exp Color
light c
  = let RGBA r g b a            = unlift c
    in  clampColor $ rawColor (r + 0.2) (g + 0.2) (b + 0.2) a

-- | Darken a color, adding black.
dark :: Exp Color -> Exp Color
dark c
  = let RGBA r g b a            = unlift c
    in  clampColor $ rawColor (r - 0.2) (g - 0.2) (b - 0.2) a

-- | Make a colour completely opaque.
opaque :: Exp Color -> Exp Color
opaque c
  = let RGBA r g b _            = unlift c
    in  rawColor r g b 1.0


-- Pre-defined Colors ---------------------------------------------------------

-- | A greyness of a given magnitude.
--
greyN :: Exp Float      -- ^ Range is 0 = black, to 1 = white.
      -> Exp Color
greyN n         = rawColor n   n   n   1.0

black, white :: Exp Color
black           = rawColor 0.0 0.0 0.0 1.0
white           = rawColor 1.0 1.0 1.0 1.0

-- Colors from the additive color wheel.
red, green, blue :: Exp Color
red             = rawColor 1.0 0.0 0.0 1.0
green           = rawColor 0.0 1.0 0.0 1.0
blue            = rawColor 0.0 0.0 1.0 1.0

-- secondary
yellow, cyan, magenta :: Exp Color
yellow          = addColors red   green
cyan            = addColors green blue
magenta         = addColors red   blue

-- tertiary
rose, violet, azure, aquamarine, chartreuse, orange :: Exp Color
rose            = addColors red     magenta
violet          = addColors magenta blue
azure           = addColors blue    cyan
aquamarine      = addColors cyan    green
chartreuse      = addColors green   yellow
orange          = addColors yellow  red

