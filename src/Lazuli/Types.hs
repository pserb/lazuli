module Lazuli.Types
  ( Color(..)
  , Field
  , ScalarField
  , ColorField
  , Seed
  , Palette
  , lerpColor
  , lerpColorRGB
  , clampChannel
  , black
  , white
  , constColor
  , applyPalette
  , clamp01
  , addColors
  , scaleColor
  ) where

type Field a = (Double, Double) -> a
type ScalarField = Field Double
type ColorField = Field Color
type Seed = Int
type Palette = Double -> Color

data Color = Color { r :: !Double, g :: !Double, b :: !Double, a :: !Double }
  deriving (Show, Eq)

clampChannel :: Double -> Double
clampChannel = max 0 . min 1
{-# INLINE clampChannel #-}

-- | Linear RGB interpolation (fast, for performance-critical paths).
lerpColorRGB :: Double -> Color -> Color -> Color
lerpColorRGB t (Color r1 g1 b1 a1) (Color r2 g2 b2 a2) =
  Color (r1 + t * (r2 - r1))
        (g1 + t * (g2 - g1))
        (b1 + t * (b2 - b1))
        (a1 + t * (a2 - a1))
{-# INLINE lerpColorRGB #-}

-- | Perceptually uniform color interpolation via OKLAB.
-- This is a forward declaration; the actual implementation is in Lazuli.Color.
-- We break the import cycle by using RGB lerp here and letting Palette.hs
-- import lerpOklab directly from Lazuli.Color.
lerpColor :: Double -> Color -> Color -> Color
lerpColor = lerpColorRGB
{-# INLINE lerpColor #-}

black :: Color
black = Color 0 0 0 1

white :: Color
white = Color 1 1 1 1

constColor :: Color -> ColorField
constColor c = const c

applyPalette :: Palette -> ScalarField -> ColorField
applyPalette pal sf = \pt -> pal (sf pt)
{-# INLINE applyPalette #-}

clamp01 :: Double -> Double
clamp01 = max 0 . min 1
{-# INLINE clamp01 #-}

addColors :: Color -> Color -> Color
addColors (Color r1 g1 b1 a1) (Color r2 g2 b2 a2) =
  Color (r1 + r2) (g1 + g2) (b1 + b2) (a1 + a2)
{-# INLINE addColors #-}

scaleColor :: Double -> Color -> Color
scaleColor s (Color cr cg cb ca) = Color (cr * s) (cg * s) (cb * s) (ca * s)
{-# INLINE scaleColor #-}
