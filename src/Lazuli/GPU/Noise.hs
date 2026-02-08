{-# LANGUAGE RebindableSyntax #-}
-- | GPU-accelerated noise functions using accelerate expressions.
--
-- These mirror Lazuli.Noise but operate on @Exp@ values so they compile
-- to GPU kernels.  Simplex noise uses the standard Ashima/McEwan approach
-- ported to accelerate's expression language.
module Lazuli.GPU.Noise
  ( gSimplex
  , gWorley
  , gVoronoi
  , gFbm
  , gRidged
  , gTurbulence
  , gRidgedFbm
  , gWorleyF2F1
  ) where

import Data.Array.Accelerate as A
import Lazuli.GPU.Field (GScalarField, gClamp01)

------------------------------------------------------------------------
-- Internal hash helpers (all pure Exp computations)
------------------------------------------------------------------------

-- | Integer hash producing a float in [0,1].
hashIntF :: Exp Int -> Exp Int -> Exp Double
hashIntF a b =
  let h0 = a * 374761393 + b * 668265263
      h1 = A.xor (A.shiftR h0 13) h0 * 1274126177
      h2 = A.xor (A.shiftR h1 16) h1
  in A.fromIntegral (h2 .&. 0xFFFF) / 65535.0

-- | 2D hash returning two floats in [0,1].
hash2D :: Exp Int -> Exp Int -> Exp Int -> Exp (Double, Double)
hash2D seed ix iy =
  let h  = ix * 374761393 + iy * 668265263 + seed * 1274126177
      h1 = A.xor (A.shiftR h 13) h * 1274126177
      h2 = A.xor (A.shiftR h1 16) h1
      px = A.fromIntegral (h2 .&. 0xFFFF) / 65535.0
      py = A.fromIntegral (A.shiftR h2 16 .&. 0xFFFF) / 65535.0
  in T2 px py

------------------------------------------------------------------------
-- Simplex 2D noise (Ashima Arts / McEwan GLSL port)
------------------------------------------------------------------------

-- | Helper: mod289 for permutation hashing.
mod289 :: Exp Double -> Exp Double
mod289 x = x - A.floor (x / 289.0) * 289.0

-- | Permute for simplex noise.
permute :: Exp Double -> Exp Double
permute x = mod289 (x * 34.0 + 1.0) * x

-- | Raw 2D simplex noise, returns [-1, 1].
snoise :: Exp Double -> Exp Double -> Exp Double
snoise xin yin =
  let -- Skew constants
      f2  = 0.5 * (sqrt 3.0 - 1.0)
      g2  = (3.0 - sqrt 3.0) / 6.0
      -- Skew input
      s   = (xin + yin) * f2
      i   = A.floor (xin + s)
      j   = A.floor (yin + s)
      t   = (i + j) * g2
      x0  = xin - (i - t)
      y0  = yin - (j - t)
      -- Simplex triangle
      i1  = A.cond (x0 A.> y0) 1.0 0.0
      j1  = A.cond (x0 A.> y0) 0.0 1.0
      x1  = x0 - i1 + g2
      y1  = y0 - j1 + g2
      x2  = x0 - 1.0 + 2.0 * g2
      y2  = y0 - 1.0 + 2.0 * g2
      -- Permutation indices
      ii  = mod289 i
      jj  = mod289 j
      p0  = permute (permute jj + ii)
      p1  = permute (permute (jj + j1) + ii + i1)
      p2  = permute (permute (jj + 1.0) + ii + 1.0)
      -- Gradients via permutation (2-component pseudo-random)
      gx0 = 2.0 * fract (p0 / 41.0) - 1.0
      gy0 = abs gx0 - 0.5
      gx0' = A.floor (gx0 + 0.5)
      ox0 = gx0 - gx0'
      gx1 = 2.0 * fract (p1 / 41.0) - 1.0
      gy1 = abs gx1 - 0.5
      gx1' = A.floor (gx1 + 0.5)
      ox1 = gx1 - gx1'
      gx2 = 2.0 * fract (p2 / 41.0) - 1.0
      gy2 = abs gx2 - 0.5
      gx2' = A.floor (gx2 + 0.5)
      ox2 = gx2 - gx2'
      -- Normalisation factor
      norm0 = 1.79284291400159 - 0.85373472095314 * (ox0*ox0 + gy0*gy0)
      norm1 = 1.79284291400159 - 0.85373472095314 * (ox1*ox1 + gy1*gy1)
      norm2 = 1.79284291400159 - 0.85373472095314 * (ox2*ox2 + gy2*gy2)
      -- Radial falloff
      t0r   = 0.5 - x0*x0 - y0*y0
      t0    = A.max t0r 0.0
      n0    = A.cond (t0r A.< 0) 0 (t0*t0*t0*t0 * norm0 * (ox0 * x0 + gy0 * y0))
      t1r   = 0.5 - x1*x1 - y1*y1
      t1    = A.max t1r 0.0
      n1    = A.cond (t1r A.< 0) 0 (t1*t1*t1*t1 * norm1 * (ox1 * x1 + gy1 * y1))
      t2r   = 0.5 - x2*x2 - y2*y2
      t2    = A.max t2r 0.0
      n2    = A.cond (t2r A.< 0) 0 (t2*t2*t2*t2 * norm2 * (ox2 * x2 + gy2 * y2))
  in 130.0 * (n0 + n1 + n2)
  where
    fract :: Exp Double -> Exp Double
    fract v = v - A.floor v

------------------------------------------------------------------------
-- Public noise functions
------------------------------------------------------------------------

-- | Seeded simplex noise. Frequency scales input; seed offsets domain.
-- Output: [0, 1].
gSimplex :: Exp Int -> Exp Double -> GScalarField
gSimplex seed freq pt =
  let T2 x y = pt
      x' = x * freq + A.fromIntegral seed * 17.31
      y' = y * freq + A.fromIntegral seed * 43.57
  in gClamp01 ((snoise x' y' + 1.0) / 2.0)

-- | Worley noise: distance to nearest cell point. Output: [0, 1].
gWorley :: Exp Int -> Exp Int -> GScalarField
gWorley seed n pt =
  let T2 x y = pt
      nf  = A.fromIntegral n
      cx  = A.floor (x * nf) :: Exp Int
      cy  = A.floor (y * nf) :: Exp Int
      -- Check 3x3 neighborhood (unrolled)
      dist ix iy =
        let T2 px py = hash2D seed ix iy
            rx = (A.fromIntegral ix + px) / nf
            ry = (A.fromIntegral iy + py) / nf
            dx = x - rx
            dy = y - ry
        in sqrt (dx*dx + dy*dy)
      -- Minimum of all 9 neighbors
      d00 = dist (cx-1) (cy-1); d01 = dist (cx-1) cy; d02 = dist (cx-1) (cy+1)
      d10 = dist cx     (cy-1); d11 = dist cx     cy; d12 = dist cx     (cy+1)
      d20 = dist (cx+1) (cy-1); d21 = dist (cx+1) cy; d22 = dist (cx+1) (cy+1)
      minDist = A.min d00 $ A.min d01 $ A.min d02 $
                A.min d10 $ A.min d11 $ A.min d12 $
                A.min d20 $ A.min d21 d22
  in gClamp01 (minDist * nf)

-- | Voronoi cell ID: hash of nearest cell point. Output: [0, 1].
gVoronoi :: Exp Int -> Exp Int -> GScalarField
gVoronoi seed n pt =
  let T2 x y = pt
      nf  = A.fromIntegral n
      cx  = A.floor (x * nf) :: Exp Int
      cy  = A.floor (y * nf) :: Exp Int
      -- For each neighbor, compute (dist, cellId)
      cell ix iy =
        let T2 px py = hash2D seed ix iy
            rx = (A.fromIntegral ix + px) / nf
            ry = (A.fromIntegral iy + py) / nf
            dx = x - rx; dy = y - ry
        in T2 (dx*dx + dy*dy) px
      -- Find nearest
      closer (T2 d1 c1) (T2 d2 c2) = A.cond (d1 A.< d2) (T2 d1 c1) (T2 d2 c2)
      c00 = cell (cx-1) (cy-1); c01 = cell (cx-1) cy; c02 = cell (cx-1) (cy+1)
      c10 = cell cx     (cy-1); c11 = cell cx     cy; c12 = cell cx     (cy+1)
      c20 = cell (cx+1) (cy-1); c21 = cell (cx+1) cy; c22 = cell (cx+1) (cy+1)
      T2 _ cid = closer c00 $ closer c01 $ closer c02 $
                 closer c10 $ closer c11 $ closer c12 $
                 closer c20 $ closer c21 c22
  in cid

-- | Fractal Brownian Motion: layered simplex octaves. Output: [0, 1].
gFbm :: Exp Int -> Exp Int -> Exp Double -> GScalarField
gFbm octaves seed freq pt =
  -- Unroll up to 8 octaves (covers all usage in styles)
  let go 0 _ _ _ total maxV = T2 total maxV
      go i s f amp (T2 tot mv) =
        let val = gSimplex s f pt
        in go (i - 1 :: Int) (s + 1) (f * 2) (amp * 0.5) (T2 (tot + val * amp) (mv + amp))
      -- We use a fixed unroll since accelerate doesn't have true loops at Exp level.
      -- Instead, build up to the max octave count with conditional accumulation.
      step i s f amp tot mv =
        let val  = gSimplex (seed + A.constant i) (freq * (2 ** A.fromIntegral (A.constant i :: Exp Int))) pt
            aval = (0.5 ** A.fromIntegral (A.constant i :: Exp Int))
            tot' = tot + val * aval
            mv'  = mv + aval
        in A.cond (A.constant i A.< octaves) (T2 tot' mv') (T2 tot mv)
      T2 t0 m0 = step 0 seed freq 1.0 0 0
      T2 t1 m1 = step 1 (seed+1) (freq*2) 0.5 t0 m0
      T2 t2 m2 = step 2 (seed+2) (freq*4) 0.25 t1 m1
      T2 t3 m3 = step 3 (seed+3) (freq*8) 0.125 t2 m2
      T2 t4 m4 = step 4 (seed+4) (freq*16) 0.0625 t3 m3
      T2 t5 m5 = step 5 (seed+5) (freq*32) 0.03125 t4 m4
      T2 t6 m6 = step 6 (seed+6) (freq*64) 0.015625 t5 m5
      T2 t7 m7 = step 7 (seed+7) (freq*128) 0.0078125 t6 m6
  in gClamp01 (t7 / A.max m7 1e-10)

-- | Ridged noise: sharp ridges from simplex. Output: [0, 1].
gRidged :: Exp Int -> Exp Double -> GScalarField
gRidged seed freq pt =
  let raw = gSimplex seed freq pt
  in 1.0 - abs (2.0 * raw - 1.0)

-- | Turbulence: absolute-value FBM. Output: [0, 1].
gTurbulence :: Exp Int -> Exp Int -> Exp Double -> GScalarField
gTurbulence octaves seed freq pt =
  let step i tot mv =
        let val  = gSimplex (seed + A.constant i) (freq * (2 ** A.fromIntegral (A.constant i :: Exp Int))) pt
            aval = 0.5 ** A.fromIntegral (A.constant i :: Exp Int)
            tot' = tot + abs (2.0 * val - 1.0) * aval
            mv'  = mv + aval
        in A.cond (A.constant i A.< octaves) (T2 tot' mv') (T2 tot mv)
      T2 t0 m0 = step 0 0 0
      T2 t1 m1 = step 1 t0 m0
      T2 t2 m2 = step 2 t1 m1
      T2 t3 m3 = step 3 t2 m2
      T2 t4 m4 = step 4 t3 m3
      T2 t5 m5 = step 5 t4 m4
      T2 t6 m6 = step 6 t5 m5
      T2 t7 m7 = step 7 t6 m6
  in gClamp01 (t7 / A.max m7 1e-10)

-- | Ridged FBM with feedback weighting. Output: [0, 1].
gRidgedFbm :: Exp Int -> Exp Double -> Exp Int -> Exp Double -> GScalarField
gRidgedFbm octaves gain seed freq pt =
  let -- Each octave: raw -> ridged -> squared -> weight feedback
      step i w tot mv =
        let val  = gSimplex (seed + A.constant i) (freq * (2 ** A.fromIntegral (A.constant i :: Exp Int))) pt
            aval = 0.5 ** A.fromIntegral (A.constant i :: Exp Int)
            n0   = 1.0 - abs (2.0 * val - 1.0)
            n1   = n0 * n0
            n2   = n1 * w
            w'   = gClamp01 (n2 * gain)
            tot' = tot + n2 * aval
            mv'  = mv + aval
        in A.cond (A.constant i A.< octaves) (T3 w' tot' mv') (T3 w tot mv)
      T3 w0 t0 m0 = step 0 1.0 0 0
      T3 w1 t1 m1 = step 1 w0 t0 m0
      T3 w2 t2 m2 = step 2 w1 t1 m1
      T3 w3 t3 m3 = step 3 w2 t2 m2
      T3 w4 t4 m4 = step 4 w3 t3 m3
      T3 w5 t5 m5 = step 5 w4 t4 m4
      T3 w6 t6 m6 = step 6 w5 t5 m5
      T3 _  t7 m7 = step 7 w6 t6 m6
  in gClamp01 (t7 / A.max m7 1e-10)

-- | Worley F2-F1 edge detection. Output: [0, 1].
gWorleyF2F1 :: Exp Int -> Exp Int -> GScalarField
gWorleyF2F1 seed n pt =
  let T2 x y = pt
      nf  = A.fromIntegral n
      cx  = A.floor (x * nf) :: Exp Int
      cy  = A.floor (y * nf) :: Exp Int
      dist ix iy =
        let T2 px py = hash2D seed ix iy
            rx = (A.fromIntegral ix + px) / nf
            ry = (A.fromIntegral iy + py) / nf
            dx = x - rx; dy = y - ry
        in sqrt (dx*dx + dy*dy)
      -- Track two smallest distances
      update (T2 f1 f2) d = A.cond (d A.< f1) (T2 d f1)
                              (A.cond (d A.< f2) (T2 f1 d) (T2 f1 f2))
      z = T2 (1e10 :: Exp Double) (1e10 :: Exp Double)
      r1 = update z  (dist (cx-1) (cy-1))
      r2 = update r1 (dist (cx-1) cy)
      r3 = update r2 (dist (cx-1) (cy+1))
      r4 = update r3 (dist cx     (cy-1))
      r5 = update r4 (dist cx     cy)
      r6 = update r5 (dist cx     (cy+1))
      r7 = update r6 (dist (cx+1) (cy-1))
      r8 = update r7 (dist (cx+1) cy)
      r9 = update r8 (dist (cx+1) (cy+1))
      T2 f1 f2 = r9
  in gClamp01 ((f2 - f1) * nf)
