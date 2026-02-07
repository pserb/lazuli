module Lazuli.Patterns
  ( radialGradient
  , linearGradient
  , stripes
  , ripple
  , spiral
  , checkerboard
  ) where

import Lazuli.Types

-- | Distance from center point, normalized to [0, 1].
-- 0.7071 ~ sqrt(0.5), the max distance from (0.5,0.5) to a corner.
radialGradient :: (Double, Double) -> ScalarField
radialGradient (cx, cy) = \(x, y) ->
  let dist = sqrt ((x - cx) ** 2 + (y - cy) ** 2)
  in min 1.0 (dist / 0.7071)
{-# INLINE radialGradient #-}

-- | Gradient along an angle (radians), centered at (0.5, 0.5).
linearGradient :: Double -> ScalarField
linearGradient angle = \(x, y) ->
  let dx = cos angle
      dy = sin angle
      t = (x - 0.5) * dx + (y - 0.5) * dy
  in t + 0.5

-- | Smooth parallel stripes at a given frequency and angle.
stripes :: Double -> Double -> ScalarField
stripes freq angle = \(x, y) ->
  let dx = cos angle
      dy = sin angle
      t = (x * dx + y * dy) * freq
  in (sin (t * 2 * pi) + 1) / 2
{-# INLINE stripes #-}

-- | Concentric rings from a center point.
ripple :: (Double, Double) -> Double -> ScalarField
ripple (cx, cy) freq = \(x, y) ->
  let dist = sqrt ((x - cx) ** 2 + (y - cy) ** 2)
  in (sin (dist * freq * 2 * pi) + 1) / 2

-- | Spiral arms from a center point.
spiral :: (Double, Double) -> Int -> ScalarField
spiral (cx, cy) arms = \(x, y) ->
  let dx = x - cx
      dy = y - cy
      ang = atan2 dy dx
      dist = sqrt (dx ** 2 + dy ** 2)
      t = ang * fromIntegral arms / (2 * pi) + dist * 10
  in (sin (t * 2 * pi) + 1) / 2

-- | Alternating 0/1 checkerboard at given frequency.
checkerboard :: Double -> ScalarField
checkerboard freq = \(x, y) ->
  let ix = floor (x * freq) :: Int
      iy = floor (y * freq) :: Int
  in if even (ix + iy) then 0 else 1
