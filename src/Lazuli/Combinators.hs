module Lazuli.Combinators
  ( -- Field transforms (ScalarField -> ScalarField)
    clampField
  , threshold
  , smoothstep
  , invert
  , remap
  , absField
  , power
    -- Spatial transforms (Field a -> Field a)
  , rotate
  , scale
  , translate
  , tile
  , mirrorX
  , mirrorY
  , toPolar
    -- Domain warping
  , warp
    -- Blending (ColorField operations)
  , blend
  , mask
  , add
  , multiply
  , screen
  , over
  ) where

import Lazuli.Types (Color (..), ColorField, Field, ScalarField, lerpColor)

--------------------------------------------------------------------------------
-- Field transforms (ScalarField -> ScalarField)
--------------------------------------------------------------------------------

-- | Clamp values to [0, 1]
clampField :: ScalarField -> ScalarField
clampField f = \p -> max 0 (min 1 (f p))

-- | Hard threshold: below t -> 0, above t -> 1
threshold :: Double -> ScalarField -> ScalarField
threshold t f = \p -> if f p < t then 0 else 1

-- | Smooth hermite interpolation between two edges
smoothstep :: Double -> Double -> ScalarField -> ScalarField
smoothstep edge0 edge1 f = \p ->
  let x = max 0 (min 1 ((f p - edge0) / (edge1 - edge0)))
  in x * x * (3 - 2 * x)

-- | Invert: 1 - f(x,y)
invert :: ScalarField -> ScalarField
invert f = \p -> 1 - f p

-- | Remap [lo, hi] to [0, 1]
remap :: Double -> Double -> ScalarField -> ScalarField
remap lo hi f = \p -> (f p - lo) / (hi - lo)

-- | Absolute value
absField :: ScalarField -> ScalarField
absField f = \p -> abs (f p)

-- | Power: raise values to a power (contrast control)
power :: Double -> ScalarField -> ScalarField
power n f = \p -> f p ** n

--------------------------------------------------------------------------------
-- Spatial transforms (Field a -> Field a)
--------------------------------------------------------------------------------

-- | Rotate by angle (radians) around center (0.5, 0.5)
rotate :: Double -> Field a -> Field a
rotate angle f = \(x, y) ->
  let cx = x - 0.5
      cy = y - 0.5
      cosA = cos (-angle)
      sinA = sin (-angle)
      x' = cx * cosA - cy * sinA + 0.5
      y' = cx * sinA + cy * cosA + 0.5
  in f (x', y')

-- | Scale from center (0.5, 0.5)
scale :: Double -> Double -> Field a -> Field a
scale sx sy f = \(x, y) ->
  let x' = (x - 0.5) / sx + 0.5
      y' = (y - 0.5) / sy + 0.5
  in f (x', y')

-- | Translate
translate :: Double -> Double -> Field a -> Field a
translate dx dy f = \(x, y) -> f (x - dx, y - dy)

-- | Tile: repeat in a grid
tile :: Int -> Int -> Field a -> Field a
tile nx ny f = \(x, y) ->
  let x' = mod' (x * fromIntegral nx) 1.0
      y' = mod' (y * fromIntegral ny) 1.0
  in f (x', y')
  where
    mod' a b = a - b * fromIntegral (floor (a / b) :: Int)

-- | Mirror around x=0.5
mirrorX :: Field a -> Field a
mirrorX f = \(x, y) -> f (1 - x, y)

-- | Mirror around y=0.5
mirrorY :: Field a -> Field a
mirrorY f = \(x, y) -> f (x, 1 - y)

-- | Convert to polar coordinates before sampling.
-- Maps (x,y) -> (angle/2pi, distance) then samples the field.
toPolar :: Field a -> Field a
toPolar f = \(x, y) ->
  let dx = x - 0.5
      dy = y - 0.5
      angle = atan2 dy dx / (2 * pi) + 0.5
      dist = sqrt (dx * dx + dy * dy) * 2
  in f (angle, dist)

--------------------------------------------------------------------------------
-- Domain warping
--------------------------------------------------------------------------------

-- | Warp: use two scalar fields to displace the input coordinates
warp :: ScalarField -> ScalarField -> Double -> Field a -> Field a
warp dxF dyF strength f = \(x, y) ->
  let dx = dxF (x, y) * strength
      dy = dyF (x, y) * strength
  in f (x + dx, y + dy)

--------------------------------------------------------------------------------
-- Blending / Composition (ColorField operations)
--------------------------------------------------------------------------------

-- | Linear interpolation: t=0 -> field a, t=1 -> field b
blend :: Double -> ColorField -> ColorField -> ColorField
blend t fa fb = \p -> lerpColor t (fa p) (fb p)

-- | Use a scalar field as a mask: 0 -> field a, 1 -> field b
mask :: ScalarField -> ColorField -> ColorField -> ColorField
mask m fa fb = \p ->
  let t = max 0 (min 1 (m p))
  in lerpColor t (fa p) (fb p)

-- | Additive blending
add :: ColorField -> ColorField -> ColorField
add fa fb = \p ->
  let Color r1 g1 b1 a1 = fa p
      Color r2 g2 b2 a2 = fb p
  in Color (min 1 (r1 + r2)) (min 1 (g1 + g2)) (min 1 (b1 + b2)) (min 1 (a1 + a2))

-- | Multiply (darkens)
multiply :: ColorField -> ColorField -> ColorField
multiply fa fb = \p ->
  let Color r1 g1 b1 a1 = fa p
      Color r2 g2 b2 a2 = fb p
  in Color (r1 * r2) (g1 * g2) (b1 * b2) (a1 * a2)

-- | Screen (lightens): 1 - (1-a)*(1-b)
screen :: ColorField -> ColorField -> ColorField
screen fa fb = \p ->
  let Color r1 g1 b1 a1 = fa p
      Color r2 g2 b2 a2 = fb p
  in Color (1 - (1 - r1) * (1 - r2))
           (1 - (1 - g1) * (1 - g2))
           (1 - (1 - b1) * (1 - b2))
           (1 - (1 - a1) * (1 - a2))

-- | Alpha compositing: layer foreground over background
over :: ColorField -> ColorField -> ColorField
over fg bg = \p ->
  let Color fr fg' fb' fa' = fg p
      Color br bg' bb' ba' = bg p
      outA = fa' + ba' * (1 - fa')
      outR = if outA == 0 then 0 else (fr * fa' + br * ba' * (1 - fa')) / outA
      outG = if outA == 0 then 0 else (fg' * fa' + bg' * ba' * (1 - fa')) / outA
      outB = if outA == 0 then 0 else (fb' * fa' + bb' * ba' * (1 - fa')) / outA
  in Color outR outG outB outA
