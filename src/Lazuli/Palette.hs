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

------------------------------------------------------------------------
-- Named palettes
------------------------------------------------------------------------

-- Warm muted sunset: deep dusk → dusty rose → warm amber → pale cream
sunsetPalette :: Palette
sunsetPalette = fromStops
  [ (0.0,  Color 0.15 0.08 0.18 1)
  , (0.3,  Color 0.55 0.22 0.25 1)
  , (0.6,  Color 0.82 0.48 0.18 1)
  , (1.0,  Color 0.95 0.85 0.68 1)
  ]

-- Deep ocean: unchanged — dark navy → teal → pale seafoam
oceanPalette :: Palette
oceanPalette = fromStops
  [ (0.0, Color 0.02 0.05 0.2  1)
  , (0.5, Color 0.0  0.5  0.55 1)
  , (1.0, Color 0.9  0.95 0.98 1)
  ]

-- Muted neon: deep midnight → subdued purple → dark teal → midnight
cyberpunkPalette :: Palette
cyberpunkPalette = fromStops
  [ (0.0,  Color 0.04 0.02 0.12 1)
  , (0.3,  Color 0.30 0.08 0.42 1)
  , (0.7,  Color 0.08 0.35 0.45 1)
  , (1.0,  Color 0.04 0.02 0.12 1)
  ]

-- Moss: unchanged — dark olive → sage → pale beige
mossPalette :: Palette
mossPalette = fromStops
  [ (0.0, Color 0.1  0.25 0.05 1)
  , (0.5, Color 0.55 0.65 0.45 1)
  , (1.0, Color 0.95 0.92 0.85 1)
  ]

-- Warm embers: deep char → ember red → burnt sienna → warm gold
firePalette :: Palette
firePalette = fromStops
  [ (0.0,  Color 0.06 0.02 0.01 1)
  , (0.3,  Color 0.48 0.10 0.04 1)
  , (0.6,  Color 0.72 0.32 0.08 1)
  , (1.0,  Color 0.88 0.68 0.35 1)
  ]

-- Ice: unchanged — white → light blue → deep blue → black
icePalette :: Palette
icePalette = fromStops
  [ (0.0, Color 1.0  1.0  1.0  1)
  , (0.3, Color 0.7  0.85 1.0  1)
  , (0.7, Color 0.1  0.2  0.6  1)
  , (1.0, Color 0.0  0.0  0.0  1)
  ]

-- Dusk: soft lavender → dusty mauve → muted steel → pale lavender
duskPalette :: Palette
duskPalette = fromStops
  [ (0.0,  Color 0.20 0.16 0.30 1)
  , (0.35, Color 0.42 0.30 0.48 1)
  , (0.65, Color 0.28 0.38 0.48 1)
  , (1.0,  Color 0.68 0.62 0.70 1)
  ]

-- Monochrome: black → white
monochromePalette :: Palette
monochromePalette = fromStops
  [ (0.0, Color 0.0 0.0 0.0 1)
  , (1.0, Color 1.0 1.0 1.0 1)
  ]

-- Muted thermal: deep black → warm purple → copper → amber → warm white
infraredPalette :: Palette
infraredPalette = fromStops
  [ (0.0,  Color 0.03 0.02 0.06 1)
  , (0.25, Color 0.28 0.06 0.30 1)
  , (0.5,  Color 0.55 0.18 0.10 1)
  , (0.75, Color 0.78 0.52 0.18 1)
  , (1.0,  Color 0.92 0.88 0.80 1)
  ]

-- Soft aurora: deep night → muted green → teal → muted violet
auroraPalette :: Palette
auroraPalette = fromStops
  [ (0.0,  Color 0.04 0.05 0.18 1)
  , (0.3,  Color 0.10 0.40 0.28 1)
  , (0.6,  Color 0.08 0.35 0.40 1)
  , (1.0,  Color 0.28 0.15 0.38 1)
  ]

-- Magma: warm peach → dark → golden olive (Quilez cosine, 3/4 cycle)
-- c scaled to 0.75 so pal(0) ≠ pal(1), enabling correct --invert behavior.
magmaPalette :: Palette
magmaPalette = cosinePalette
  (0.5, 0.5, 0.5) (0.5, 0.5, 0.5) (0.75, 0.525, 0.3) (0.0, 0.15, 0.20)

-- Emerald: warm cream → amber → deep blue → sky blue (Quilez cosine, 3/4 cycle)
-- c scaled to 0.75 so pal(0) ≠ pal(1), enabling correct --invert behavior.
emeraldPalette :: Palette
emeraldPalette = cosinePalette
  (0.5, 0.5, 0.5) (0.5, 0.5, 0.5) (0.75, 0.75, 0.75) (0.0, 0.10, 0.20)

-- Twilight: muted warm-to-cool transition, no hot pinks
-- Lower amplitude prevents garish saturation
twilightPalette :: Palette
twilightPalette = fromStops
  [ (0.0,  Color 0.52 0.35 0.22 1)
  , (0.3,  Color 0.42 0.22 0.35 1)
  , (0.6,  Color 0.18 0.15 0.40 1)
  , (1.0,  Color 0.08 0.06 0.22 1)
  ]

-- Slate: cool sophisticated grays with subtle blue undertone
slatePalette :: Palette
slatePalette = fromStops
  [ (0.0,  Color 0.08 0.09 0.12 1)
  , (0.3,  Color 0.22 0.24 0.30 1)
  , (0.6,  Color 0.45 0.48 0.55 1)
  , (1.0,  Color 0.78 0.80 0.84 1)
  ]

------------------------------------------------------------------------
-- Registry
------------------------------------------------------------------------

allPalettes :: [(String, Palette)]
allPalettes =
  [ ("sunset",     sunsetPalette)
  , ("ocean",      oceanPalette)
  , ("cyberpunk",  cyberpunkPalette)
  , ("moss",       mossPalette)
  , ("fire",       firePalette)
  , ("ice",        icePalette)
  , ("dusk",       duskPalette)
  , ("monochrome", monochromePalette)
  , ("infrared",   infraredPalette)
  , ("aurora",     auroraPalette)
  , ("magma",      magmaPalette)
  , ("emerald",    emeraldPalette)
  , ("twilight",   twilightPalette)
  , ("slate",      slatePalette)
  ]

paletteByName :: String -> Maybe Palette
paletteByName name = lookup name allPalettes

paletteNames :: [String]
paletteNames = map fst allPalettes
