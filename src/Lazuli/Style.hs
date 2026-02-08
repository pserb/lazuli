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

-- | Fluted glass: architectural glass panels with smooth gradient behind them.
-- Perfectly straight, evenly-spaced vertical panels with 3D convex shading,
-- dark gaps, and subtle chromatic refraction.
flutedStyle :: Style
flutedStyle seed pal freqMul =
  vignette 0.15 finalImage
  where
    f = freqMul

    -- SMOOTH background: low-frequency simplex, NOT fbm with warping.
    -- A gentle color wash that the glass panels structure.
    bgSmooth = simplex seed (0.8 * f)
    bgVariation = simplex (seed + 1) (0.5 * f)
    background = \(x, y) ->
      let base = bgSmooth (x, y)
          vary = bgVariation (x, y) * 0.2
          t = clamp01 (base * 0.7 + vary + 0.15)
      in pal t

    -- Panel parameters
    numPanels = 20.0 * f
    gapWidth = 0.008 / f  -- thin dark gap between panels

    finalImage (x, y) =
      let -- Which panel are we in? Simple division, NO noise on position
          panelFloat = x * numPanels
          panelIndex = floor panelFloat :: Int
          -- Position within panel: 0.0 = left edge, 1.0 = right edge
          posInPanel = panelFloat - fromIntegral panelIndex

          -- Gap detection: are we in the dark gap between panels?
          halfGap = gapWidth * numPanels * 0.5
          isGap = posInPanel < halfGap
                  || posInPanel > (1.0 - halfGap)

          -- 3D panel shading: convex shape, bright center, dark edges
          centered = posInPanel * 2.0 - 1.0
          panelShading = 1.0 - 0.3 * centered * centered

          -- Subtle refraction: smooth sinusoidal displacement per panel position
          refractionStrength = 0.015
          dx = sin (posInPanel * pi) * refractionStrength
          -- Chromatic dispersion: tiny per-channel offset
          dxR = dx * 0.8
          dxG = dx * 1.0
          dxB = dx * 1.25

          -- Sample the smooth background at slightly different x positions per channel
          Color rR _ _ _ = background (x + dxR, y)
          Color _ gG _ _ = background (x + dxG, y)
          Color _ _ bB _ = background (x + dxB, y)

          -- Apply panel shading
          shadedR = rR * panelShading
          shadedG = gG * panelShading
          shadedB = bB * panelShading

      in if isGap
         then let Color cr cg cb _ = background (x, y)
              in Color (cr * 0.15) (cg * 0.15) (cb * 0.15) 1.0
         else Color (min 1.0 shadedR) (min 1.0 shadedG) (min 1.0 shadedB) 1.0

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
