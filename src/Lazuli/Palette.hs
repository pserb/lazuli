module Lazuli.Palette
  ( fromStops
  , cosinePalette
  , paletteByName
  , allPalettes
  , paletteNames
  ) where

import Lazuli.Types (Color(..), Palette, clampChannel)
import Lazuli.Color (lerpOklab)
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
          in lerpOklab localT (getColor idx) (getColor (idx + 1))
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

-- Warm sunset: deep plum dusk → dusty rose → warm coral → amber → pale cream
sunsetPalette :: Palette
sunsetPalette = fromStops
  [ (0.0,  Color 0.12 0.06 0.16 1)
  , (0.2,  Color 0.35 0.12 0.22 1)
  , (0.4,  Color 0.58 0.24 0.22 1)
  , (0.6,  Color 0.78 0.42 0.18 1)
  , (0.8,  Color 0.90 0.68 0.38 1)
  , (1.0,  Color 0.96 0.88 0.72 1)
  ]

-- Deep ocean: abyssal dark → deep navy → teal → aquamarine → pale seafoam
oceanPalette :: Palette
oceanPalette = fromStops
  [ (0.0,  Color 0.01 0.03 0.14 1)
  , (0.25, Color 0.03 0.10 0.32 1)
  , (0.5,  Color 0.02 0.38 0.48 1)
  , (0.75, Color 0.30 0.72 0.70 1)
  , (1.0,  Color 0.85 0.94 0.96 1)
  ]

-- Cyberpunk: deep void → indigo → electric purple → dark teal → void
cyberpunkPalette :: Palette
cyberpunkPalette = fromStops
  [ (0.0,  Color 0.03 0.01 0.10 1)
  , (0.2,  Color 0.14 0.04 0.30 1)
  , (0.4,  Color 0.34 0.08 0.46 1)
  , (0.6,  Color 0.12 0.30 0.48 1)
  , (0.8,  Color 0.06 0.14 0.28 1)
  , (1.0,  Color 0.03 0.01 0.10 1)
  ]

-- Moss: deep forest floor → dark olive → sage → warm green → pale beige
mossPalette :: Palette
mossPalette = fromStops
  [ (0.0,  Color 0.06 0.12 0.03 1)
  , (0.25, Color 0.18 0.30 0.10 1)
  , (0.5,  Color 0.42 0.52 0.30 1)
  , (0.75, Color 0.68 0.72 0.52 1)
  , (1.0,  Color 0.94 0.91 0.84 1)
  ]

-- Fire: deep char → ember → crimson → burnt orange → warm gold → pale amber
firePalette :: Palette
firePalette = fromStops
  [ (0.0,  Color 0.05 0.01 0.01 1)
  , (0.2,  Color 0.32 0.06 0.02 1)
  , (0.4,  Color 0.56 0.12 0.04 1)
  , (0.6,  Color 0.74 0.30 0.06 1)
  , (0.8,  Color 0.86 0.54 0.18 1)
  , (1.0,  Color 0.92 0.74 0.40 1)
  ]

-- Ice: bright white → pale blue → steel blue → deep blue → navy → black
icePalette :: Palette
icePalette = fromStops
  [ (0.0,  Color 1.0  1.0  1.0  1)
  , (0.2,  Color 0.82 0.90 0.98 1)
  , (0.4,  Color 0.52 0.68 0.88 1)
  , (0.6,  Color 0.20 0.36 0.68 1)
  , (0.8,  Color 0.06 0.12 0.38 1)
  , (1.0,  Color 0.0  0.0  0.0  1)
  ]

-- Dusk: deep indigo → dusty violet → mauve → muted steel → soft lavender → pale lilac
duskPalette :: Palette
duskPalette = fromStops
  [ (0.0,  Color 0.14 0.10 0.26 1)
  , (0.2,  Color 0.28 0.18 0.38 1)
  , (0.4,  Color 0.42 0.30 0.46 1)
  , (0.6,  Color 0.34 0.38 0.50 1)
  , (0.8,  Color 0.54 0.52 0.62 1)
  , (1.0,  Color 0.72 0.68 0.76 1)
  ]

-- Monochrome: black → white
monochromePalette :: Palette
monochromePalette = fromStops
  [ (0.0, Color 0.0 0.0 0.0 1)
  , (1.0, Color 1.0 1.0 1.0 1)
  ]

-- Infrared: deep void → warm purple → magenta → copper → amber → warm white
infraredPalette :: Palette
infraredPalette = fromStops
  [ (0.0,  Color 0.02 0.01 0.05 1)
  , (0.15, Color 0.18 0.04 0.22 1)
  , (0.3,  Color 0.38 0.08 0.32 1)
  , (0.5,  Color 0.58 0.18 0.12 1)
  , (0.7,  Color 0.76 0.44 0.14 1)
  , (0.85, Color 0.88 0.68 0.38 1)
  , (1.0,  Color 0.94 0.90 0.82 1)
  ]

-- Aurora: deep night → dark green → emerald → teal → blue-violet → muted violet
auroraPalette :: Palette
auroraPalette = fromStops
  [ (0.0,  Color 0.03 0.04 0.14 1)
  , (0.2,  Color 0.06 0.22 0.16 1)
  , (0.4,  Color 0.10 0.42 0.28 1)
  , (0.6,  Color 0.08 0.36 0.44 1)
  , (0.8,  Color 0.18 0.20 0.42 1)
  , (1.0,  Color 0.30 0.14 0.38 1)
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

-- Twilight: warm gold → muted rose → dusty purple → deep indigo → dark night
twilightPalette :: Palette
twilightPalette = fromStops
  [ (0.0,  Color 0.56 0.38 0.22 1)
  , (0.25, Color 0.48 0.28 0.32 1)
  , (0.5,  Color 0.34 0.20 0.40 1)
  , (0.75, Color 0.16 0.12 0.34 1)
  , (1.0,  Color 0.06 0.04 0.18 1)
  ]

-- Slate: deep charcoal → cool gray → blue-gray → silver → pale steel
slatePalette :: Palette
slatePalette = fromStops
  [ (0.0,  Color 0.06 0.07 0.10 1)
  , (0.25, Color 0.16 0.18 0.24 1)
  , (0.5,  Color 0.32 0.35 0.42 1)
  , (0.75, Color 0.55 0.58 0.64 1)
  , (1.0,  Color 0.80 0.82 0.86 1)
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
