module Lazuli.Color
  ( linearRGBtoOKLAB
  , oklabToLinearRGB
  , linearRGBtoOKLCH
  , oklchToLinearRGB
  , lerpOklab
  ) where

import Lazuli.Types (Color(..), clampChannel)

-- | Convert linear RGB color to OKLAB (L, a, b) tuple.
linearRGBtoOKLAB :: Color -> (Double, Double, Double)
linearRGBtoOKLAB (Color cr cg cb _) =
  let -- Step 1: Linear RGB -> LMS
      l = 0.4122214708 * cr + 0.5363325363 * cg + 0.0514459929 * cb
      m = 0.2119034982 * cr + 0.6806995451 * cg + 0.1073969566 * cb
      s = 0.0883024619 * cr + 0.2817188376 * cg + 0.6299787005 * cb
      -- Step 2: Cube root
      l' = cbrt l
      m' = cbrt m
      s' = cbrt s
      -- Step 3: LMS' -> Lab
      labL =  0.2104542553 * l' + 0.7936177850 * m' - 0.0040720468 * s'
      labA =  1.9779984951 * l' - 2.4285922050 * m' + 0.4505937099 * s'
      labB =  0.0259040371 * l' + 0.7827717662 * m' - 0.8086757660 * s'
  in (labL, labA, labB)
{-# INLINE linearRGBtoOKLAB #-}

-- | Convert OKLAB (L, a, b) tuple to linear RGB color.
oklabToLinearRGB :: (Double, Double, Double) -> Color
oklabToLinearRGB (labL, labA, labB) =
  let -- Step 1: Lab -> LMS' (M2_inv)
      l' = labL + 0.3963377774 * labA + 0.2158037573 * labB
      m' = labL - 0.1055613458 * labA - 0.0638541728 * labB
      s' = labL - 0.0894841775 * labA - 1.2914855480 * labB
      -- Step 2: Cube
      l = l' * l' * l'
      m = m' * m' * m'
      s = s' * s' * s'
      -- Step 3: LMS -> Linear RGB (M1_inv)
      cr =  4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s
      cg = -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s
      cb = -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s
  in Color (clampChannel cr) (clampChannel cg) (clampChannel cb) 1.0
{-# INLINE oklabToLinearRGB #-}

-- | Convert linear RGB to OKLCH (L, C, H) tuple.
linearRGBtoOKLCH :: Color -> (Double, Double, Double)
linearRGBtoOKLCH c =
  let (l, labA, labB) = linearRGBtoOKLAB c
      chroma = sqrt (labA * labA + labB * labB)
      hue = atan2 labB labA
  in (l, chroma, hue)
{-# INLINE linearRGBtoOKLCH #-}

-- | Convert OKLCH (L, C, H) tuple to linear RGB color.
oklchToLinearRGB :: (Double, Double, Double) -> Color
oklchToLinearRGB (l, chroma, hue) =
  let labA = chroma * cos hue
      labB = chroma * sin hue
  in oklabToLinearRGB (l, labA, labB)
{-# INLINE oklchToLinearRGB #-}

-- | Perceptually uniform color interpolation in OKLAB space.
-- Alpha is interpolated linearly.
lerpOklab :: Double -> Color -> Color -> Color
lerpOklab t c1 c2 =
  let (l1, a1, b1) = linearRGBtoOKLAB c1
      (l2, a2, b2) = linearRGBtoOKLAB c2
      l = l1 + t * (l2 - l1)
      la = a1 + t * (a2 - a1)
      lb = b1 + t * (b2 - b1)
      result = oklabToLinearRGB (l, la, lb)
      alpha = a c1 + t * (a c2 - a c1)
  in result { a = alpha }
{-# INLINE lerpOklab #-}

-- | Cube root that handles negative values.
cbrt :: Double -> Double
cbrt x
  | x >= 0    = x ** (1/3)
  | otherwise = -((-x) ** (1/3))
{-# INLINE cbrt #-}
