module Lazuli.Style
  ( Style
  , styleByName
  , allStyles
  , styleNames
  ) where

import Lazuli.Types (Seed, Palette, ColorField, Color(..), applyPalette, constColor, black)
import Lazuli.Noise (simplex, worley, voronoi, fbm, ridged)
import Lazuli.Patterns (radialGradient, stripes, spiral)
import Lazuli.Combinators (warp, blend, mask, threshold, power)

type Style = Seed -> Palette -> ColorField

allStyles :: [(String, String, Style)]
allStyles =
  [ ("voronoi",      "Warped voronoi cells",           voronoiStyle)
  , ("nebula",       "Layered fractal noise + worley",  nebulaStyle)
  , ("crystal",      "Ridged noise with hard edges",    crystalStyle)
  , ("domainwarp",   "Multi-layer domain warping",      domainWarpStyle)
  , ("flow",         "Flow field visualization",        flowStyle)
  , ("stainedglass", "Voronoi cells with borders",      stainedGlassStyle)
  ]

styleByName :: String -> Maybe Style
styleByName name = case filter (\(n,_,_) -> n == name) allStyles of
  ((_,_,s):_) -> Just s
  []          -> Nothing

styleNames :: [String]
styleNames = map (\(n,_,_) -> n) allStyles

-- Style implementations

voronoiStyle :: Style
voronoiStyle seed pal =
  applyPalette pal
    $ warp (simplex seed 0.003) (simplex (seed+1) 0.003) 80.0
    $ voronoi seed 30

nebulaStyle :: Style
nebulaStyle seed pal =
  blend 0.6
    (applyPalette pal $ fbm 6 seed 0.002)
    (applyPalette pal $ warp n1 n2 60.0 $ worley seed 20)
  where
    n1 = simplex (seed+1) 0.004
    n2 = simplex (seed+2) 0.004

crystalStyle :: Style
crystalStyle seed pal =
  applyPalette pal
    $ power 1.5
    $ ridged seed 0.005

domainWarpStyle :: Style
domainWarpStyle seed pal =
  applyPalette pal
    $ warp l2x l2y 120.0
    $ warp l1x l1y 80.0
    $ simplex seed 0.003
  where
    l1x = simplex (seed+1) 0.004
    l1y = simplex (seed+2) 0.004
    l2x = simplex (seed+3) 0.003
    l2y = simplex (seed+4) 0.003

flowStyle :: Style
flowStyle seed pal =
  applyPalette pal
    $ mask (radialGradient (0.5, 0.5))
      (fbm 4 seed 0.005)
      (warp (spiral (0.5, 0.5) 5) (simplex seed 0.003) 40.0
        $ stripes 0.01 (pi/4))

stainedGlassStyle :: Style
stainedGlassStyle seed pal =
  mask borders
    (constColor black)
    (applyPalette pal $ voronoi seed 40)
  where
    borders = threshold 0.02 $ worley seed 40
