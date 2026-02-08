{-# LANGUAGE RebindableSyntax #-}
-- | Core GPU types and field abstraction for accelerate-based rendering.
--
-- In the CPU path a field is @(Double, Double) -> a@.
-- On the GPU we instead use @Exp (Double, Double) -> Exp a@ which accelerate
-- can compile to parallel GPU code.
module Lazuli.GPU.Field
  ( -- * Types
    GColor(..)
  , GScalarField
  , GColorField
  , GPalette
    -- * Color helpers
  , gColor
  , gBlack
  , gLerpColor
  , gAddColors
  , gScaleColor
  , gClamp01
  , gApplyPalette
  ) where

import Data.Array.Accelerate as A

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | GPU-side color: four linear-space doubles.
-- Stored as a tuple for unboxed accelerate representation.
type GColor = (Double, Double, Double, Double)

-- | A scalar field on the GPU: takes a normalized 2D point, returns [0,1].
type GScalarField = Exp (Double, Double) -> Exp Double

-- | A color field on the GPU: takes a normalized 2D point, returns RGBA.
type GColorField = Exp (Double, Double) -> Exp GColor

-- | A palette on the GPU: maps [0,1] to a color.
-- Implemented as a lookup into a precomputed 1D array.
type GPalette = Exp Double -> Exp GColor

------------------------------------------------------------------------
-- Color construction and helpers
------------------------------------------------------------------------

gColor :: Exp Double -> Exp Double -> Exp Double -> Exp Double -> Exp GColor
gColor r g b a' = T4 r g b a'

gBlack :: Exp GColor
gBlack = T4 0 0 0 1

gLerpColor :: Exp Double -> Exp GColor -> Exp GColor -> Exp GColor
gLerpColor t (T4 r1 g1 b1 a1) (T4 r2 g2 b2 a2) =
  T4 (r1 + t * (r2 - r1))
     (g1 + t * (g2 - g1))
     (b1 + t * (b2 - b1))
     (a1 + t * (a2 - a1))

gAddColors :: Exp GColor -> Exp GColor -> Exp GColor
gAddColors (T4 r1 g1 b1 a1) (T4 r2 g2 b2 a2) =
  T4 (r1 + r2) (g1 + g2) (b1 + b2) (a1 + a2)

gScaleColor :: Exp Double -> Exp GColor -> Exp GColor
gScaleColor s (T4 r g b a') = T4 (r * s) (g * s) (b * s) (a' * s)

gClamp01 :: Exp Double -> Exp Double
gClamp01 v = A.max 0 (A.min 1 v)

-- | Build a GPU palette from a precomputed accelerate array of 256 color stops.
-- Uses linear interpolation between array entries.
gApplyPalette :: Acc (Vector GColor) -> GPalette
gApplyPalette palArr t =
  let n     = 255 :: Exp Int
      tc    = gClamp01 t
      fi    = tc * A.fromIntegral n
      idx   = A.floor fi :: Exp Int
      frac  = fi - A.fromIntegral idx
      idx0  = A.max 0 (A.min n idx)
      idx1  = A.min n (idx0 + 1)
      c0    = palArr ! I1 idx0
      c1    = palArr ! I1 idx1
  in gLerpColor frac c0 c1
