{-# LANGUAGE RebindableSyntax #-}
-- | GPU-accelerated field combinators mirroring Lazuli.Combinators.
module Lazuli.GPU.Combinators
  ( -- * Scalar field transforms
    gSmoothstep
  , gPower
    -- * Domain warping
  , gWarp
    -- * Color blending
  , gScreen
  , gMask
    -- * Post-processing
  , gVignette
  ) where

import Data.Array.Accelerate as A
import Lazuli.GPU.Field

------------------------------------------------------------------------
-- Scalar field transforms
------------------------------------------------------------------------

-- | Smooth hermite interpolation between two edges.
gSmoothstep :: Exp Double -> Exp Double -> GScalarField -> GScalarField
gSmoothstep edge0 edge1 f pt =
  let x = gClamp01 ((f pt - edge0) / (edge1 - edge0))
  in x * x * (3 - 2 * x)

-- | Power: raise values to a power (contrast control).
gPower :: Exp Double -> GScalarField -> GScalarField
gPower n f pt = f pt ** n

------------------------------------------------------------------------
-- Domain warping
------------------------------------------------------------------------

-- | Warp: displace field coordinates using two scalar displacement fields.
gWarp :: GScalarField -> GScalarField -> Exp Double -> GScalarField -> GScalarField
gWarp dxF dyF strength f pt =
  let T2 x y = pt
      dx = dxF pt * strength
      dy = dyF pt * strength
  in f (T2 (x + dx) (y + dy))

-- | Warp variant for color fields.
gWarpColor :: GScalarField -> GScalarField -> Exp Double -> GColorField -> GColorField
gWarpColor dxF dyF strength f pt =
  let T2 x y = pt
      dx = dxF pt * strength
      dy = dyF pt * strength
  in f (T2 (x + dx) (y + dy))

------------------------------------------------------------------------
-- Color blending
------------------------------------------------------------------------

-- | Screen blend: 1 - (1-a)*(1-b). Lightens.
gScreen :: GColorField -> GColorField -> GColorField
gScreen fa fb pt =
  let T4 r1 g1 b1 a1 = fa pt
      T4 r2 g2 b2 a2 = fb pt
  in T4 (1 - (1-r1)*(1-r2)) (1 - (1-g1)*(1-g2)) (1 - (1-b1)*(1-b2)) (1 - (1-a1)*(1-a2))

-- | Mask: use a scalar field to blend between two color fields.
gMask :: GScalarField -> GColorField -> GColorField -> GColorField
gMask m fa fb pt =
  let t = gClamp01 (m pt)
  in gLerpColor t (fa pt) (fb pt)

------------------------------------------------------------------------
-- Post-processing (applied at field level, before rasterization)
------------------------------------------------------------------------

-- | Radial edge darkening.
gVignette :: Exp Double -> GColorField -> GColorField
gVignette strength cf pt =
  let T2 x y = pt
      dx = x - 0.5
      dy = y - 0.5
      dist = sqrt (dx*dx + dy*dy)
      maxDist = 0.7071
      t = A.min 1.0 (dist / maxDist)
      s = gClamp01 ((t - 0.3) / 0.7)
      falloff = s * s * (3 - 2 * s)
      darkening = 1.0 - strength * falloff
      T4 cr cg cb ca = cf pt
  in T4 (cr * darkening) (cg * darkening) (cb * darkening) ca
