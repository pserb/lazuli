module Lazuli.Palette
  ( fromStops
  , paletteByName
  , allPalettes
  , paletteNames
  ) where

import Lazuli.Types (Color(..), Palette, lerpColor)

-- | Build a palette from color stops: [(position, color)]
-- Positions are in [0, 1]. The list should be sorted by position.
-- For t between two stops, linearly interpolate between their colors.
-- For t <= first stop position, return first color.
-- For t >= last stop position, return last color.
fromStops :: [(Double, Color)] -> Palette
fromStops stops t
  | t <= fst (head stops) = snd (head stops)
  | t >= fst (last stops) = snd (last stops)
  | otherwise =
    let pairs = zip stops (tail stops)
        ((p1, c1), (p2, c2)) = head $ filter (\((a,_),(b,_)) -> t >= a && t <= b) pairs
        localT = (t - p1) / (p2 - p1)
    in lerpColor localT c1 c2

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
  ]

paletteByName :: String -> Maybe Palette
paletteByName name = lookup name allPalettes

paletteNames :: [String]
paletteNames = map fst allPalettes
