module Lazuli.Noise
  ( simplex
  , worley
  , voronoi
  , fbm
  , ridged
  , turbulence
  , ridgedFbm
  , worleyF2F1
  ) where

import Data.Bits (xor, shiftR, (.&.))
import Data.List (foldl')
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Lazuli.Types

-- | 2D simplex noise. Frequency scales input coordinates.
-- Output normalized to [0, 1].
simplex :: Seed -> Double -> ScalarField
simplex seed freq = \(x, y) ->
  let x' = x * freq
      y' = y * freq
      raw = simplex2D perm x' y'
  in clamp01 ((raw + 1) / 2)
  where
    perm = buildPermTable seed

-- | Distance to nearest random point in an NxN grid.
-- Output normalized to [0, 1].
worley :: Seed -> Int -> ScalarField
worley seed n = \(x, y) ->
  let nf = fromIntegral n
      cx = floor (x * nf) :: Int
      cy = floor (y * nf) :: Int
      minDist = minimum
        [ let (px, py) = hash2D seed ix iy
              rx = (fromIntegral ix + px) / nf
              ry = (fromIntegral iy + py) / nf
              dx = x - rx
              dy = y - ry
          in sqrt (dx * dx + dy * dy)
        | ix <- [cx - 1 .. cx + 1]
        , iy <- [cy - 1 .. cy + 1]
        ]
  in clamp01 (minDist * nf)

-- | Voronoi cell ID — hash of nearest point's cell, normalized to [0, 1].
voronoi :: Seed -> Int -> ScalarField
voronoi seed n = \(x, y) ->
  let nf = fromIntegral n
      cx = floor (x * nf) :: Int
      cy = floor (y * nf) :: Int
      cells =
        [ let (px, py) = hash2D seed ix iy
              rx = (fromIntegral ix + px) / nf
              ry = (fromIntegral iy + py) / nf
              dx = x - rx
              dy = y - ry
          in (dx * dx + dy * dy, ix, iy)
        | ix <- [cx - 1 .. cx + 1]
        , iy <- [cy - 1 .. cy + 1]
        ]
      (_, nearX, nearY) = minimum cells
      (cellId, _) = hash2D seed nearX nearY
  in cellId

-- | Fractal Brownian Motion — layer multiple octaves of simplex noise.
-- Output normalized to [0, 1].
-- Perm tables and amplitude weights are pre-built once, then reused for all pixels.
fbm :: Int -> Seed -> Double -> ScalarField
fbm octaves seed freq =
  let -- Pre-build all octave closures (each with its own perm table) OUTSIDE the pixel lambda
      octaveFns = [ simplex (seed + i) (freq * (2 ^^ i)) | i <- [0 .. octaves - 1] ]
      amps      = [ 0.5 ^^ i | i <- [0 .. octaves - 1] :: [Int] ]
      maxVal    = sum amps
  in \(x, y) ->
    let total = sumWith (\sf amp -> sf (x, y) * amp) octaveFns amps
    in clamp01 (total / maxVal)

-- | Ridged multifractal — creates sharp ridges from simplex noise.
-- Output normalized to [0, 1].
ridged :: Seed -> Double -> ScalarField
ridged seed freq =
  let sf = simplex seed freq
  in \(x, y) ->
    let raw = sf (x, y)
    in 1.0 - abs (2.0 * raw - 1.0)

-- | Turbulence — absolute-value fBM. Creates sharp valleys at zero crossings.
-- Ideal for marble/agate veining when fed into sin().
-- Output normalized to [0, 1].
turbulence :: Int -> Seed -> Double -> ScalarField
turbulence octaves seed freq =
  let octaveFns = [ simplex (seed + i) (freq * (2 ^^ i)) | i <- [0 .. octaves - 1] ]
      amps      = [ 0.5 ^^ i | i <- [0 .. octaves - 1] :: [Int] ]
      maxVal    = sum amps
  in \(x, y) ->
    let total = sumWith (\sf amp -> abs (2.0 * sf (x, y) - 1.0) * amp) octaveFns amps
    in clamp01 (total / maxVal)

-- | Multi-octave ridged noise with feedback weighting.
-- Ridges self-reinforce while valleys become smooth, creating branching vein networks.
-- Parameters: octaves, gain (feedback strength), seed, frequency.
-- Output normalized to [0, 1].
ridgedFbm :: Int -> Double -> Seed -> Double -> ScalarField
ridgedFbm octaves gain seed freq =
  let -- Pre-build: octave k (0-indexed) uses seed+k, freq * 2^k, amp = 0.5^k
      octaveData = [ (simplex (seed + k) (freq * (2 ^^ k)), 0.5 ^^ k :: Double)
                   | k <- [0 .. octaves - 1] ]
      maxVal = sum [ 0.5 ^^ i | i <- [0 .. octaves - 1] :: [Int] ]
  in \(x, y) ->
    let go _ [] sig = sig
        go !w ((sf, amp):rest) sig =
          let raw = sf (x, y)
              n0  = 1.0 - abs (2.0 * raw - 1.0)
              n1  = n0 * n0
              n2  = n1 * w
              w'  = clamp01 (n2 * gain)
          in go w' rest (sig + n2 * amp)
        total = go 1.0 octaveData 0.0
    in clamp01 (total / maxVal)

-- | F2-F1 Voronoi edge detection.
-- Returns the difference between second-nearest and nearest cell distances.
-- Value is 0 at cell edges, larger inside cells.
-- Output normalized to [0, 1].
worleyF2F1 :: Seed -> Int -> ScalarField
worleyF2F1 seed n = \(x, y) ->
  let nf = fromIntegral n
      cx = floor (x * nf) :: Int
      cy = floor (y * nf) :: Int
      dists =
        [ let (px, py) = hash2D seed ix iy
              rx = (fromIntegral ix + px) / nf
              ry = (fromIntegral iy + py) / nf
              dx = x - rx
              dy = y - ry
          in sqrt (dx * dx + dy * dy)
        | ix <- [cx - 1 .. cx + 1]
        , iy <- [cy - 1 .. cy + 1]
        ]
      -- Find two smallest distances
      (f1, f2) = foldl' updateF1F2 (1e10, 1e10) dists
  in clamp01 ((f2 - f1) * nf)
  where
    updateF1F2 (d1, d2) d
      | d < d1    = (d, d1)
      | d < d2    = (d1, d)
      | otherwise = (d1, d2)

------------------------------------------------------------------------
-- Internal: Simplex 2D
------------------------------------------------------------------------

-- Skew / unskew constants for 2D simplex
f2 :: Double
f2 = 0.5 * (sqrt 3 - 1)

g2 :: Double
g2 = (3 - sqrt 3) / 6

-- Gradient table (12 entries, cycling for mod 12)
grad2 :: Vector (Double, Double)
grad2 = V.fromList
  [ ( 1, 1), (-1, 1), ( 1,-1), (-1,-1)
  , ( 1, 0), (-1, 0), ( 0, 1), ( 0,-1)
  , ( 1, 1), (-1, 1), ( 1,-1), (-1,-1)
  ]

dot2 :: (Double, Double) -> Double -> Double -> Double
dot2 (gx, gy) x y = gx * x + gy * y
{-# INLINE dot2 #-}

-- Build a 512-entry permutation table (256 doubled) from a seed.
buildPermTable :: Seed -> Vector Int
buildPermTable seed = V.generate 512 (\i -> V.unsafeIndex base (i `mod` 256))
  where
    base = shuffleWith seed (V.generate 256 id)

-- Fisher–Yates shuffle using a deterministic seed-derived sequence.
shuffleWith :: Seed -> Vector Int -> Vector Int
shuffleWith seed0 vec = V.modify (\mv -> go mv (V.length vec - 1) seed0) vec
  where
    go _  0 _    = pure ()
    go mv i seed = do
      let h = hashInt seed i
          j = abs h `mod` (i + 1)
      vi <- MV.unsafeRead mv i
      vj <- MV.unsafeRead mv j
      MV.unsafeWrite mv i vj
      MV.unsafeWrite mv j vi
      go mv (i - 1) (seed + 1)

hashInt :: Int -> Int -> Int
hashInt a b =
  let h0 = a * 374761393 + b * 668265263
      h1 = (h0 `xor` (h0 `shiftR` 13)) * 1274126177
  in h1 `xor` (h1 `shiftR` 16)
{-# INLINE hashInt #-}

hash2D :: Seed -> Int -> Int -> (Double, Double)
hash2D seed ix iy =
  let h = ix * 374761393 + iy * 668265263 + seed * 1274126177
      h1 = (h `xor` (h `shiftR` 13)) * 1274126177
      h2 = h1 `xor` (h1 `shiftR` 16)
  in ( fromIntegral (h2 .&. 0xFFFF) / 65535.0
     , fromIntegral ((h2 `shiftR` 16) .&. 0xFFFF) / 65535.0
     )
{-# INLINE hash2D #-}

-- Core 2D simplex noise, returns [-1, 1].
simplex2D :: Vector Int -> Double -> Double -> Double
simplex2D perm xin yin =
  let -- Skew input space
      s = (xin + yin) * f2
      i = floor (xin + s) :: Int
      j = floor (yin + s) :: Int
      t = fromIntegral (i + j) * g2
      x0 = xin - (fromIntegral i - t)
      y0 = yin - (fromIntegral j - t)

      -- Determine which simplex triangle
      (i1, j1) = if x0 > y0 then (1, 0) else (0, 1)

      x1 = x0 - fromIntegral i1 + g2
      y1 = y0 - fromIntegral j1 + g2
      x2 = x0 - 1.0 + 2.0 * g2
      y2 = y0 - 1.0 + 2.0 * g2

      ii = i .&. 255
      jj = j .&. 255

      gi0 = V.unsafeIndex perm (ii + V.unsafeIndex perm jj) `mod` 12
      gi1 = V.unsafeIndex perm (ii + i1 + V.unsafeIndex perm (jj + j1)) `mod` 12
      gi2 = V.unsafeIndex perm (ii + 1 + V.unsafeIndex perm (jj + 1)) `mod` 12

      -- Contribution from corner 0
      t0 = 0.5 - x0 * x0 - y0 * y0
      n0 = if t0 < 0 then 0
           else let t0' = t0 * t0
                in t0' * t0' * dot2 (V.unsafeIndex grad2 gi0) x0 y0

      -- Contribution from corner 1
      t1 = 0.5 - x1 * x1 - y1 * y1
      n1 = if t1 < 0 then 0
           else let t1' = t1 * t1
                in t1' * t1' * dot2 (V.unsafeIndex grad2 gi1) x1 y1

      -- Contribution from corner 2
      t2 = 0.5 - x2 * x2 - y2 * y2
      n2 = if t2 < 0 then 0
           else let t2' = t2 * t2
                in t2' * t2' * dot2 (V.unsafeIndex grad2 gi2) x2 y2

  in 70.0 * (n0 + n1 + n2)
{-# INLINE simplex2D #-}

-- | Strict zip-sum: avoids per-pixel list allocation for fbm/turbulence.
sumWith :: (a -> b -> Double) -> [a] -> [b] -> Double
sumWith f = go 0.0
  where
    go !acc (a:as) (b:bs) = go (acc + f a b) as bs
    go !acc _      _      = acc
{-# INLINE sumWith #-}
