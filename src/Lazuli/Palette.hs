module Lazuli.Palette
  ( fromStops
  , cosinePalette
  , paletteByName
  , allPalettes
  , paletteNames
  ) where

import Lazuli.Types (Color(..), Palette, lerpColor, clampChannel)
import qualified Data.Vector.Unboxed as VU

-- | Build a palette from color stops: [(position, color)]
-- Positions are in [0, 1]. The list should be sorted by position.
-- Uses binary search for O(log n) lookup instead of O(n) list traversal.
fromStops :: [(Double, Color)] -> Palette
fromStops stops = let
    n = length stops
    positions = VU.fromListN n (map fst stops)
    -- Store colors as flat (r,g,b,a) tuples in an unboxed vector
    colors = VU.fromListN n (map (\(_, Color cr cg cb ca) -> (cr, cg, cb, ca)) stops)
    getColor i = let (cr, cg, cb, ca) = VU.unsafeIndex colors i in Color cr cg cb ca
  in \t ->
    if t <= VU.unsafeIndex positions 0
      then getColor 0
      else if t >= VU.unsafeIndex positions (n - 1)
        then getColor (n - 1)
        else
          -- Binary search for the interval containing t
          let idx = bsearch positions t
              p1 = VU.unsafeIndex positions idx
              p2 = VU.unsafeIndex positions (idx + 1)
              localT = (t - p1) / (p2 - p1)
          in lerpColor localT (getColor idx) (getColor (idx + 1))
{-# INLINE fromStops #-}

-- | Binary search: find largest index i where positions[i] <= t and t <= positions[i+1].
bsearch :: VU.Vector Double -> Double -> Int
bsearch vec t = go 0 (VU.length vec - 2)
  where
    go lo hi
      | lo >= hi  = lo
      | otherwise =
          let mid = (lo + hi + 1) `div` 2
          in if VU.unsafeIndex vec mid <= t
               then go mid hi
               else go lo (mid - 1)
{-# INLINE bsearch #-}

-- | Cosine palette (Inigo Quilez). Produces infinitely smooth gradients.
-- color(t) = a + b * cos(2*pi * (c*t + d))
-- Parameters a, b, c, d are RGB triples controlling bias, amplitude, frequency, phase.
cosinePalette :: (Double, Double, Double)  -- ^ a (RGB bias)
              -> (Double, Double, Double)  -- ^ b (RGB amplitude)
              -> (Double, Double, Double)  -- ^ c (RGB frequency)
              -> (Double, Double, Double)  -- ^ d (RGB phase)
              -> Palette
cosinePalette (ar,ag,ab) (br,bg,bb) (cr,cg,cb) (dr,dg,db) t =
  let f v a' b' c' d' = clampChannel (a' + b' * cos (2 * pi * (c' * v + d')))
  in Color (f t ar br cr dr) (f t ag bg cg dg) (f t ab bb cb db) 1.0
{-# INLINE cosinePalette #-}

-- Named palettes

sunsetPalette :: Palette
sunsetPalette = fromStops
  [ (0.0, Color 0.55 0.15 0.05 1)
  , (0.4, Color 1.0  0.45 0.0  1)
  , (0.7, Color 0.85 0.1  0.55 1)
  , (1.0, Color 0.3  0.05 0.35 1)
  ]

oceanPalette :: Palette
oceanPalette = fromStops
  [ (0.0, Color 0.02 0.05 0.2  1)
  , (0.5, Color 0.0  0.5  0.55 1)
  , (1.0, Color 0.9  0.95 0.98 1)
  ]

cyberpunkPalette :: Palette
cyberpunkPalette = fromStops
  [ (0.0, Color 0.0  0.0  0.0  1)
  , (0.3, Color 1.0  0.05 0.5  1)
  , (0.7, Color 0.0  0.9  1.0  1)
  , (1.0, Color 0.0  0.0  0.0  1)
  ]

mossPalette :: Palette
mossPalette = fromStops
  [ (0.0, Color 0.1  0.25 0.05 1)
  , (0.5, Color 0.55 0.65 0.45 1)
  , (1.0, Color 0.95 0.92 0.85 1)
  ]

firePalette :: Palette
firePalette = fromStops
  [ (0.0, Color 0.0  0.0  0.0  1)
  , (0.3, Color 0.8  0.05 0.0  1)
  , (0.6, Color 1.0  0.55 0.0  1)
  , (1.0, Color 1.0  0.95 0.2  1)
  ]

icePalette :: Palette
icePalette = fromStops
  [ (0.0, Color 1.0  1.0  1.0  1)
  , (0.3, Color 0.7  0.85 1.0  1)
  , (0.7, Color 0.1  0.2  0.6  1)
  , (1.0, Color 0.0  0.0  0.0  1)
  ]

vaporwavePalette :: Palette
vaporwavePalette = fromStops
  [ (0.0,  Color 1.0  0.15 0.55 1)
  , (0.33, Color 0.5  0.1  0.8  1)
  , (0.66, Color 0.0  0.75 0.7  1)
  , (1.0,  Color 1.0  0.15 0.55 1)
  ]

monochromePalette :: Palette
monochromePalette = fromStops
  [ (0.0, Color 0.0 0.0 0.0 1)
  , (1.0, Color 1.0 1.0 1.0 1)
  ]

infraredPalette :: Palette
infraredPalette = fromStops
  [ (0.0,  Color 0.0  0.0  0.0  1)
  , (0.25, Color 0.45 0.0  0.6  1)
  , (0.5,  Color 0.85 0.1  0.05 1)
  , (0.75, Color 1.0  0.9  0.1  1)
  , (1.0,  Color 1.0  1.0  1.0  1)
  ]

auroraPalette :: Palette
auroraPalette = fromStops
  [ (0.0,  Color 0.05 0.05 0.35 1)
  , (0.33, Color 0.1  0.8  0.3  1)
  , (0.66, Color 0.0  0.65 0.6  1)
  , (1.0,  Color 0.5  0.15 0.7  1)
  ]

-- Cosine palettes (Quilez formula)

magmaPalette :: Palette
magmaPalette = cosinePalette
  (0.5, 0.5, 0.5) (0.5, 0.5, 0.5) (1.0, 0.7, 0.4) (0.0, 0.15, 0.20)

emeraldPalette :: Palette
emeraldPalette = cosinePalette
  (0.5, 0.5, 0.5) (0.5, 0.5, 0.5) (1.0, 1.0, 1.0) (0.0, 0.10, 0.20)

twilightPalette :: Palette
twilightPalette = cosinePalette
  (0.5, 0.5, 0.5) (0.5, 0.5, 0.5) (2.0, 1.0, 0.0) (0.50, 0.20, 0.25)

rainbowPalette :: Palette
rainbowPalette = cosinePalette
  (0.5, 0.5, 0.5) (0.5, 0.5, 0.5) (1.0, 1.0, 1.0) (0.0, 0.33, 0.67)

allPalettes :: [(String, Palette)]
allPalettes =
  [ ("sunset",     sunsetPalette)
  , ("ocean",      oceanPalette)
  , ("cyberpunk",  cyberpunkPalette)
  , ("moss",       mossPalette)
  , ("fire",       firePalette)
  , ("ice",        icePalette)
  , ("vaporwave",  vaporwavePalette)
  , ("monochrome", monochromePalette)
  , ("infrared",   infraredPalette)
  , ("aurora",     auroraPalette)
  , ("magma",      magmaPalette)
  , ("emerald",    emeraldPalette)
  , ("twilight",   twilightPalette)
  , ("rainbow",    rainbowPalette)
  ]

paletteByName :: String -> Maybe Palette
paletteByName name = lookup name allPalettes

paletteNames :: [String]
paletteNames = map fst allPalettes
