module Lazuli.Style
  ( Style
  , styleByName
  , allStyles
  , styleNames
  ) where

import Data.Bits (xor, shiftR, (.&.))
import Lazuli.Types (Seed, Palette, ColorField, Color(..), applyPalette, constColor,
                     clamp01, lerpColor)
import Lazuli.Noise (simplex, fbm, ridgedFbm, turbulence)
import Lazuli.Combinators (warp, mask, power, smoothstep, screen, vignette)

-- | A style takes a seed, palette, and frequency multiplier (1.0 = default).
-- The frequency multiplier scales all internal noise frequencies, controlling
-- spatial density of features. Lower = broader/smoother, higher = tighter/denser.
type Style = Seed -> Palette -> Double -> ColorField

allStyles :: [(String, String, Style)]
allStyles =
  [ ("crystal",      "Ridged crystal vein networks",        crystalStyle)
  , ("domainwarp",   "IQ triple-pass domain warping",       domainWarpStyle)
  , ("ink",          "Curl noise fluid dynamics",           inkStyle)
  , ("fluted",       "Fluted glass distortion",             flutedStyle)
  , ("magma",        "Turbulence-driven marble and lava",   magmaStyle)
  , ("mesh",         "Smooth mesh gradient blobs",          meshStyle)
  ]

styleByName :: String -> Maybe Style
styleByName name = case filter (\(n,_,_) -> n == name) allStyles of
  ((_,_,s):_) -> Just s
  []          -> Nothing

styleNames :: [String]
styleNames = map (\(n,_,_) -> n) allStyles

------------------------------------------------------------------------
-- Styles (all accept frequency multiplier)
------------------------------------------------------------------------

-- | Multi-octave ridged noise with cross-hatching, smoothstepped facet edges,
-- light domain warping, and radial vignette.
crystalStyle :: Style
crystalStyle seed pal freqMul =
  vignette 0.35 colored
  where
    f = freqMul
    veins = ridgedFbm 5 0.9 seed (4.0 * f)
    veins2 = ridgedFbm 4 0.8 (seed+10) (7.0 * f)
    combined = \pt ->
      let v1 = veins pt
          v2 = veins2 pt
      in clamp01 (v1 * 0.7 + v2 * 0.3)
    faceted = smoothstep 0.15 0.85 combined
    warped = warp (simplex (seed+5) (5.0 * f)) (simplex (seed+6) (5.0 * f)) 0.08 faceted
    shaped = power 1.3 warped
    colored = applyPalette pal shaped

-- | Inigo Quilez triple-pass domain warping.
domainWarpStyle :: Style
domainWarpStyle seed pal freqMul =
  let freq = 1.5 * freqMul
      amp  = 4.0
      -- Pre-build all fbm closures (with perm tables) BEFORE the per-pixel lambda
      fbm0 = fbm 3 seed       freq
      fbm1 = fbm 3 (seed + 1) freq
      fbm2 = fbm 3 (seed + 2) freq
      fbm3 = fbm 3 (seed + 3) freq
      fbm4 = fbm 3 (seed + 4) freq
  in \(x, y) ->
    let qx = fbm0 (x, y)
        qy = fbm1 (x + 5.2, y + 1.3)
        rx = fbm2 (x + amp * qx + 1.7, y + amp * qy + 9.2)
        ry = fbm3 (x + amp * qx + 8.3, y + amp * qy + 2.8)
        val = fbm4 (x + amp * rx, y + amp * ry)
        qLen = clamp01 (sqrt (qx * qx + qy * qy))
        baseColor = pal val
        tintColor = pal qLen
    in lerpColor (clamp01 (ry * 0.66)) baseColor tintColor

-- | Curl noise fluid dynamics — flowing ink-in-water patterns.
-- Coordinates advected through a divergence-free flow field.
inkStyle :: Style
inkStyle seed pal freqMul =
  vignette 0.25 colored
  where
    f = freqMul
    potential = fbm 3 seed (1.5 * f)
    eps = 0.002

    -- Curl of scalar potential: divergence-free flow
    curlX (x, y) =
      (potential (x, y + eps) - potential (x, y - eps)) / (2.0 * eps)
    curlY (x, y) =
      negate (potential (x + eps, y) - potential (x - eps, y)) / (2.0 * eps)

    -- Multi-pass advection through curl field
    advect (x, y) = go (8 :: Int) x y
      where
        step = 0.04
        go 0 px py = (px, py)
        go n px py =
          let vx = curlX (px, py) * step
              vy = curlY (px, py) * step
          in go (n - 1) (px + vx) (py + vy)

    -- Sample through advected coordinates
    basePattern = fbm 4 (seed + 10) (2.5 * f)
    flowField = \pt ->
      let (ax, ay) = advect pt
      in basePattern (ax, ay)

    -- Flow magnitude for color variation
    flowMag = \(x, y) ->
      let vx = curlX (x, y)
          vy = curlY (x, y)
      in clamp01 (sqrt (vx * vx + vy * vy) * 0.3)

    combined = \pt ->
      let fl = flowField pt
          m = flowMag pt
      in clamp01 (fl * 0.7 + m * 0.3)

    colored = applyPalette pal (smoothstep 0.1 0.9 combined)

-- | Fluted glass: smooth gradient viewed through cylindrical lens array.
-- Physically-based refraction with chromatic dispersion, Schlick's Fresnel,
-- Gaussian caustics, and surface imperfections.
flutedStyle :: Style
flutedStyle seed pal freqMul =
  vignette 0.2 finalImage
  where
    f = freqMul

    -- Background: smooth warped gradient (unchanged)
    bgNoise = fbm 4 seed (2.0 * f)
    bgWarped = warp (simplex (seed+1) (1.5 * f)) (simplex (seed+2) (1.5 * f)) 0.2 bgNoise
    background = applyPalette pal (power 0.8 bgWarped)

    -- Fluted glass parameters
    numRibs = 14.0 * f
    baseStrength = 0.05  -- base refraction displacement

    -- Surface imperfection: subtle noise on ridge positions
    surfaceNoise = simplex (seed+10) (30.0 * f)

    finalImage (x, y) =
      let -- Ridge phase with surface imperfection
          imperfection = surfaceNoise (x, y) * 0.02
          phase = (x + imperfection) * numRibs * 2.0 * pi

          -- Ridge shape: how far from center of current ridge
          ridgePos = abs (sin (phase / 2.0))

          -- Refraction displacement (lens curvature)
          lensDisplacement = cos phase * baseStrength

          -- Chromatic dispersion: different IOR per channel
          dxR = lensDisplacement * 0.85   -- IOR_red ~ 1.45
          dxG = lensDisplacement * 1.00   -- IOR_green ~ 1.47
          dxB = lensDisplacement * 1.20   -- IOR_blue ~ 1.49

          -- Sample background at three different positions (one per channel)
          Color rR _ _ _ = background (x + dxR, y)
          Color _ gG _ _ = background (x + dxG, y)
          Color _ _ bB _ = background (x + dxB, y)

          -- Fresnel effect (Schlick's approximation, F0 for glass ~ 0.04)
          f0 = 0.04
          cosTheta = abs (cos phase)
          fresnel = f0 + (1.0 - f0) * (1.0 - cosTheta) ** 5.0
          transmission = 1.0 - fresnel * 0.5

          -- Caustic: Gaussian-shaped light convergence at ridge centers
          causticRaw = exp (-ridgePos * ridgePos * 8.0)
          caustic = causticRaw * 0.12

          -- Combine
          finalR = min 1.0 (rR * transmission + caustic)
          finalG = min 1.0 (gG * transmission + caustic)
          finalB = min 1.0 (bB * transmission + caustic)

      in Color finalR finalG finalB 1.0

-- | Turbulence-driven marble veining with hot emission spots.
magmaStyle :: Style
magmaStyle seed pal freqMul =
  screen hotSpots base
  where
    f = freqMul
    turb = turbulence 6 seed (4.0 * f)
    marble = \(x, y) ->
      let t = turb (x, y)
          stripe = sin (x * 6.0 * f * 2 * pi + t * 10.0)
      in (stripe + 1.0) / 2.0
    warped = warp (simplex (seed+5) (3.0 * f)) (simplex (seed+6) (3.0 * f)) 0.15 marble
    shaped = power 0.8 warped
    base = applyPalette pal shaped
    hotMask = smoothstep 0.55 0.85 turb
    hotColor = applyPalette pal (power 0.5 turb)
    hotSpots = mask hotMask (constColor (Color 0 0 0 0)) hotColor

-- | Mesh gradient: anisotropic rotated Gaussian blobs with domain warping
-- for organic, designer-quality color fields.
meshStyle :: Style
meshStyle seed pal freqMul =
  vignette 0.12 meshGrad
  where
    -- More blobs for richer gradients
    numBlobs = max 4 (round (7.0 * freqMul) :: Int)

    -- Two warping fields to make blobs asymmetric and organic
    warpFieldX = simplex (seed + 100) (1.2 * freqMul)
    warpFieldY = simplex (seed + 101) (1.2 * freqMul)
    warpAmt = 0.15

    blobs = [ let (bx, _) = hash2DStyle (seed + i * 3) 0
                  (by, _) = hash2DStyle (seed + i * 3 + 1) 0
                  (ci, _) = hash2DStyle (seed + i * 3 + 2) 0
                  -- Position: keep away from exact edges
                  cx = 0.1 + bx * 0.8
                  cy = 0.1 + by * 0.8
                  -- Anisotropic sigma: each blob has different width in X vs Y
                  (aspect, _) = hash2DStyle (seed + i * 3 + 10) 0
                  baseSigma = 0.15 / freqMul + ci * (0.15 / freqMul)
                  sigmaX = baseSigma * (0.7 + aspect * 0.6)
                  sigmaY = baseSigma * (1.3 - aspect * 0.6)
                  -- Rotation angle for each blob
                  (rot, _) = hash2DStyle (seed + i * 3 + 20) 0
                  angle = rot * pi  -- 0 to pi
              in (cx, cy, sigmaX, sigmaY, angle, ci)
            | i <- [0 .. numBlobs - 1]
            ]

    meshGrad (x, y) =
      let -- Warp the coordinate space for organic flow
          wx = warpFieldX (x, y) * warpAmt
          wy = warpFieldY (x, y) * warpAmt
          x' = x + wx
          y' = y + wy

          weights = [ let -- Rotate coordinates relative to blob center
                          ddx = x' - cx
                          ddy = y' - cy
                          cosA = cos angle
                          sinA = sin angle
                          rx = ddx * cosA + ddy * sinA
                          ry = -ddx * sinA + ddy * cosA
                          -- Anisotropic Gaussian
                          d2 = (rx / sigmaX) ** 2 + (ry / sigmaY) ** 2
                          w = exp (negate d2 / 2.0)
                      in (w, palT)
                    | (cx, cy, sigmaX, sigmaY, angle, palT) <- blobs
                    ]

          totalWeight = max 1e-10 (sum (map fst weights))

          -- Weighted color blend
          blended = foldl addW (Color 0 0 0 0) weights
          addW (Color r1 g1 b1 a1) (w, palT) =
            let Color cr cg cb ca = pal palT
                s = w / totalWeight
            in Color (r1 + cr * s) (g1 + cg * s) (b1 + cb * s) (a1 + ca * s)

      in blended

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Deterministic hash for seed-based random placement.
hash2DStyle :: Seed -> Int -> (Double, Double)
hash2DStyle s i =
  let h = s * 374761393 + i * 668265263
      h1 = (h `xor` (h `shiftR` 13)) * 1274126177
      h2 = h1 `xor` (h1 `shiftR` 16)
  in ( fromIntegral (h2 .&. 0xFFFF) / 65535.0
     , fromIntegral ((h2 `shiftR` 16) .&. 0xFFFF) / 65535.0
     )
