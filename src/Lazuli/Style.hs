module Lazuli.Style
  ( Style
  , styleByName
  , allStyles
  , styleNames
  ) where

import Data.Bits (xor, shiftR, (.&.))
import Lazuli.Types (Seed, Palette, ColorField, Color(..), applyPalette, constColor,
                     clamp01, lerpColor)
import Lazuli.Noise (simplex, worley, voronoi, fbm, ridgedFbm, turbulence, worleyF2F1)
import Lazuli.Patterns (stripes)
import Lazuli.Combinators (warp, mask, power, smoothstep, invert,
                           screen, over, toPolar, vignette)

type Style = Seed -> Palette -> ColorField

allStyles :: [(String, String, Style)]
allStyles =
  [ ("voronoi",      "Warped voronoi mosaic with depth", voronoiStyle)
  , ("terrain",      "Mountain silhouette landscape",    terrainStyle)
  , ("crystal",      "Ridged crystal vein networks",     crystalStyle)
  , ("domainwarp",   "IQ triple-pass domain warping",    domainWarpStyle)
  , ("ink",          "Curl noise fluid dynamics",        inkStyle)
  , ("fluted",       "Fluted glass distortion",          flutedStyle)
  , ("magma",        "Turbulence-driven marble and lava", magmaStyle)
  , ("silk",         "Smooth organic silk folds",        silkStyle)
  , ("aurora",       "Northern lights curtains",         auroraStyle)
  , ("cells",        "Organic cell networks",            cellsStyle)
  , ("dunes",        "Sand dunes with directional light", dunesStyle)
  , ("mesh",         "Smooth mesh gradient blobs",       meshStyle)
  ]

styleByName :: String -> Maybe Style
styleByName name = case filter (\(n,_,_) -> n == name) allStyles of
  ((_,_,s):_) -> Just s
  []          -> Nothing

styleNames :: [String]
styleNames = map (\(n,_,_) -> n) allStyles

------------------------------------------------------------------------
-- Rewritten styles (were broken with pixel-space frequencies)
------------------------------------------------------------------------

-- | Warped voronoi mosaic with depth shading and interior texture.
voronoiStyle :: Style
voronoiStyle seed pal =
  vignette 0.3 colored
  where
    wx = simplex (seed + 5) 3.0
    wy = simplex (seed + 6) 3.0
    -- Voronoi cell IDs, domain-warped for organic shapes
    cellId = warp wx wy 0.12 (voronoi seed 10)
    -- Distance field for depth within cells
    cellDist = warp wx wy 0.12 (worley seed 10)
    -- Interior texture
    texture = fbm 3 (seed + 10) 6.0
    -- Combine: cell color + edge darkening + texture
    combined = \pt ->
      let cid = cellId pt
          dist = cellDist pt
          tex = texture pt
          depth = smoothstepVal 0.0 0.5 dist
      in clamp01 (cid * 0.65 + depth * 0.25 + tex * 0.1)
    colored = applyPalette pal (power 0.9 combined)

-- | Mountain silhouette landscape with atmospheric perspective.
-- Multiple ridgeline layers composited back-to-front over a sky gradient.
terrainStyle :: Style
terrainStyle seed pal =
  vignette 0.2 landscape
  where
    -- Sky gradient: palette-colored, dark at top (y=0), lighter at horizon
    skyGrad = \(_x, y) ->
      let t = smoothstepVal 0.0 0.65 y
      in pal t

    -- Compose 5 mountain layers back-to-front
    numLayers = 5
    landscape = foldl (addMountainLayer seed numLayers) skyGrad [0 .. numLayers - 1]

    addMountainLayer :: Seed -> Int -> ColorField -> Int -> ColorField
    addMountainLayer s n bg i =
      let fi = fromIntegral i
          fn = fromIntegral n
          depth = fi / (fn - 1.0)  -- 0 = far, 1 = near
          freq = 3.0 + fi * 1.5
          -- Pre-build ridgedFbm closure (with perm tables) BEFORE the per-pixel lambda
          heightProfile = ridgedFbm 4 0.85 (s + i * 7) freq
          baseHeight = 0.35 + depth * 0.3
          atmospheric = 0.3 + 0.7 * depth
          layerColor = lerpColor (1.0 - atmospheric) (pal 0.95) (pal (depth * 0.35))
          Color lr lg lb _ = layerColor
          mtnColor = Color (lr * atmospheric) (lg * atmospheric) (lb * atmospheric) 1.0
      in \(x, y) ->
        let heightRaw = heightProfile (x, 0.5)
            height = baseHeight * heightRaw
            mountainY = 1.0 - height
            edgeSoft = 0.005
            alpha = smoothstepVal (mountainY - edgeSoft) (mountainY + edgeSoft) y
            bgColor = bg (x, y)
        in lerpColor alpha bgColor mtnColor

-- | Curl noise fluid dynamics — flowing ink-in-water patterns.
-- Coordinates advected through a divergence-free flow field.
inkStyle :: Style
inkStyle seed pal =
  vignette 0.25 colored
  where
    potential = fbm 3 seed 1.5
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
    basePattern = fbm 4 (seed + 10) 2.5
    flowField = \pt ->
      let (ax, ay) = advect pt
      in basePattern (ax, ay)

    -- Flow magnitude for color variation
    flowMag = \(x, y) ->
      let vx = curlX (x, y)
          vy = curlY (x, y)
      in clamp01 (sqrt (vx * vx + vy * vy) * 0.3)

    combined = \pt ->
      let f = flowField pt
          m = flowMag pt
      in clamp01 (f * 0.7 + m * 0.3)

    colored = applyPalette pal (smoothstep 0.1 0.9 combined)

-- | Fluted glass: smooth gradient viewed through cylindrical lens array.
-- Sinusoidal UV distortion with Fresnel darkening and caustic highlights.
flutedStyle :: Style
flutedStyle seed pal =
  vignette 0.2 finalImage
  where
    -- Background: smooth warped gradient
    bgNoise = fbm 4 seed 2.0
    bgWarped = warp (simplex (seed+1) 1.5) (simplex (seed+2) 1.5) 0.2 bgNoise
    background = applyPalette pal (power 0.8 bgWarped)

    -- Fluted glass parameters
    numRibs = 12.0
    strength = 0.04

    finalImage = \(x, y) ->
      let phase = x * numRibs * 2.0 * pi
          -- Refraction displacement
          dx = cos phase * strength
          -- Sample background through displaced coords
          Color cr cg cb ca = background (x + dx, y)
          -- Fresnel: edges of ribs are darker
          ribPhase = sin phase
          fresnel = 0.7 + 0.3 * ribPhase * ribPhase
          -- Caustic highlights at rib centers
          caustic = (sin phase) ** 8 * 0.08
      in Color (min 1 (cr * fresnel + caustic))
               (min 1 (cg * fresnel + caustic))
               (min 1 (cb * fresnel + caustic)) ca

------------------------------------------------------------------------
-- Styles from round 1 (kept as-is)
------------------------------------------------------------------------

-- | Multi-octave ridged noise with cross-hatching, smoothstepped facet edges,
-- light domain warping, and radial vignette.
crystalStyle :: Style
crystalStyle seed pal =
  vignette 0.35 colored
  where
    veins = ridgedFbm 5 0.9 seed 4.0
    veins2 = ridgedFbm 4 0.8 (seed+10) 7.0
    combined = \pt ->
      let v1 = veins pt
          v2 = veins2 pt
      in clamp01 (v1 * 0.7 + v2 * 0.3)
    faceted = smoothstep 0.15 0.85 combined
    warped = warp (simplex (seed+5) 5.0) (simplex (seed+6) 5.0) 0.08 faceted
    shaped = power 1.3 warped
    colored = applyPalette pal shaped

-- | Inigo Quilez triple-pass domain warping.
domainWarpStyle :: Style
domainWarpStyle seed pal =
  let freq = 1.5
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

-- | Turbulence-driven marble veining with hot emission spots.
magmaStyle :: Style
magmaStyle seed pal =
  screen hotSpots base
  where
    turb = turbulence 6 seed 4.0
    marble = \(x, y) ->
      let t = turb (x, y)
          stripe = sin (x * 6.0 * 2 * pi + t * 10.0)
      in (stripe + 1.0) / 2.0
    warped = warp (simplex (seed+5) 3.0) (simplex (seed+6) 3.0) 0.15 marble
    shaped = power 0.8 warped
    base = applyPalette pal shaped
    hotMask = smoothstep 0.55 0.85 turb
    hotColor = applyPalette pal (power 0.5 turb)
    hotSpots = mask hotMask (constColor (Color 0 0 0 0)) hotColor

-- | Smooth organic folds with translucent layering.
silkStyle :: Style
silkStyle seed pal =
  over foreground background
  where
    baseNoise = fbm 5 seed 3.0
    dx = fbm 4 (seed+1) 2.5
    dy = fbm 4 (seed+2) 2.5
    warped = warp dx dy 0.4 baseNoise
    polarLayer = toPolar (warp dx dy 0.3 (fbm 3 (seed+3) 4.0))
    blended = \pt ->
      let w = warped pt
          p = polarLayer pt
      in clamp01 (w * 0.6 + p * 0.4)
    folded = smoothstep 0.2 0.8 blended
    background = applyPalette pal (power 2.0 folded)
    invFold = invert folded
    foreground = \pt ->
      let Color cr cg cb _ = applyPalette pal (power 0.7 invFold) pt
          alpha = (smoothstep 0.3 0.7 invFold) pt * 0.4
      in Color cr cg cb alpha

-- | Northern lights curtains screen-blended over dark sky.
auroraStyle :: Style
auroraStyle seed pal =
  screen curtains darkSky
  where
    skyNoise = power 3.0 (fbm 3 seed 2.0)
    darkSky = applyPalette (\t -> Color (t * 0.05) (t * 0.03) (t * 0.12) 1.0) skyNoise
    baseStripes = stripes 5.0 (pi / 2.0)
    warpedCurtain = warp (fbm 4 (seed+1) 2.0) (fbm 3 (seed+2) 1.5) 0.25 baseStripes
    vertFalloff = \(_x, y) ->
      let t = 1.0 - y
      in smoothstepVal 0.15 0.85 t
    curtainMask = \pt -> warpedCurtain pt * vertFalloff pt
    shimmer = turbulence 4 (seed+5) 8.0
    curtainFinal = \pt ->
      let b = curtainMask pt
          s = shimmer pt
      in clamp01 (b * (0.7 + 0.3 * s))
    curtainColored = applyPalette pal curtainFinal
    curtains = \pt ->
      let Color cr cg cb _ = curtainColored pt
          alpha = curtainFinal pt * 0.85
      in Color cr cg cb alpha

-- | Organic cell networks with thin dark borders and textured interiors.
cellsStyle :: Style
cellsStyle seed pal =
  over borderLayer cellInteriors
  where
    wx = simplex (seed+3) 3.0
    wy = simplex (seed+4) 3.0
    edges = warp wx wy 0.1 (worleyF2F1 seed 12)
    cellId = warp wx wy 0.1 (voronoi seed 12)
    internalTex = fbm 3 (seed+5) 6.0
    cellColor = \pt ->
      let cid = cellId pt
          tex = internalTex pt
      in clamp01 (cid * 0.8 + tex * 0.2)
    cellInteriors = applyPalette pal (power 0.9 cellColor)
    edgeBright = invert edges
    sharpEdges = smoothstep 0.6 0.95 edgeBright
    borderLayer = \pt ->
      let e = sharpEdges pt
      in Color 0 0 0 (e * 0.9)

------------------------------------------------------------------------
-- New styles
------------------------------------------------------------------------

-- | Sand dunes with Lambertian directional lighting.
-- Height field from fBM, surface normals via central differences.
dunesStyle :: Style
dunesStyle seed pal =
  vignette 0.3 litDunes
  where
    -- Height field: smooth dune shapes + subtle ripples
    duneShape = fbm 3 seed 2.0
    ripples = fbm 2 (seed + 5) 6.0
    heightField pt =
      let d = duneShape pt
          r = ripples pt
      in clamp01 (d * 0.85 + r * 0.15)

    -- Surface normal via central differences
    eps = 0.002
    normalAt (x, y) =
      let hL = heightField (x - eps, y)
          hR = heightField (x + eps, y)
          hD = heightField (x, y - eps)
          hU = heightField (x, y + eps)
          nx = (hL - hR) / (2.0 * eps)
          ny = (hD - hU) / (2.0 * eps)
          nz = 1.0
          len = sqrt (nx * nx + ny * ny + nz * nz)
      in (nx / len, ny / len, nz / len)

    -- Directional light: low angle from upper-left
    lightDir = let lx = 0.4; ly = 0.3; lz = 0.6
                   len = sqrt (lx*lx + ly*ly + lz*lz)
               in (lx/len, ly/len, lz/len)

    litDunes pt =
      let h = heightField pt
          (nx, ny, nz) = normalAt pt
          (lx, ly, lz) = lightDir
          diffuse = max 0.0 (nx * lx + ny * ly + nz * lz)
          brightness = 0.25 + 0.75 * diffuse
          Color cr cg cb ca = pal h
      in Color (cr * brightness) (cg * brightness) (cb * brightness) ca

-- | Apple/iOS-style mesh gradient: smooth Gaussian blobs.
-- N blobs at seed-determined positions blend with normalized weights.
meshStyle :: Style
meshStyle seed pal =
  vignette 0.15 meshGrad
  where
    numBlobs = 5 :: Int
    blobs = [ let (bx, _) = hash2DStyle (seed + i * 3) 0
                  (by, _) = hash2DStyle (seed + i * 3 + 1) 0
                  (ci, _) = hash2DStyle (seed + i * 3 + 2) 0
                  cx = 0.15 + bx * 0.7
                  cy = 0.15 + by * 0.7
                  sigma = 0.18 + ci * 0.12
              in (cx, cy, sigma, ci)
            | i <- [0 .. numBlobs - 1]
            ]

    meshGrad (x, y) =
      let weights = [ let ddx = x - cx
                          ddy = y - cy
                          d2 = ddx * ddx + ddy * ddy
                      in (exp (negate d2 / (2.0 * sigma * sigma)), palT)
                    | (cx, cy, sigma, palT) <- blobs
                    ]
          totalWeight = max 1e-10 (sum (map fst weights))
          blended = foldl addW (Color 0 0 0 0) weights
          addW (Color r1 g1 b1 a1) (w, palT) =
            let Color cr cg cb ca = pal palT
                s = w / totalWeight
            in Color (r1 + cr * s) (g1 + cg * s) (b1 + cb * s) (a1 + ca * s)
      in blended

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Smoothstep on a single value (not a field).
smoothstepVal :: Double -> Double -> Double -> Double
smoothstepVal edge0 edge1 x =
  let t = max 0 (min 1 ((x - edge0) / (edge1 - edge0)))
  in t * t * (3 - 2 * t)

-- | Deterministic hash for seed-based random placement.
hash2DStyle :: Seed -> Int -> (Double, Double)
hash2DStyle s i =
  let h = s * 374761393 + i * 668265263
      h1 = (h `xor` (h `shiftR` 13)) * 1274126177
      h2 = h1 `xor` (h1 `shiftR` 16)
  in ( fromIntegral (h2 .&. 0xFFFF) / 65535.0
     , fromIntegral ((h2 `shiftR` 16) .&. 0xFFFF) / 65535.0
     )
