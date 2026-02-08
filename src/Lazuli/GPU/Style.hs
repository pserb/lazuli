{-# LANGUAGE RebindableSyntax #-}
-- | GPU-accelerated style compositions.
--
-- Each style mirrors its CPU counterpart in Lazuli.Style but uses
-- accelerate @Exp@ combinators for GPU execution.
module Lazuli.GPU.Style
  ( GStyle
  , gStyleByName
  , gAllStyles
  ) where

import Data.Array.Accelerate as A
import Lazuli.GPU.Field
import Lazuli.GPU.Noise
import Lazuli.GPU.Combinators

------------------------------------------------------------------------
-- Style type
------------------------------------------------------------------------

-- | A GPU style: takes seed, palette array, frequency -> color field.
type GStyle = Exp Int -> Acc (Vector GColor) -> Exp Double -> GColorField

------------------------------------------------------------------------
-- Crystal
------------------------------------------------------------------------

gCrystalStyle :: GStyle
gCrystalStyle seed palArr freqMul pt =
  let pal  = gApplyPalette palArr
      f    = freqMul
      veins  = gRidgedFbm 5 0.9 seed (4.0 * f) pt
      veins2 = gRidgedFbm 4 0.8 (seed + 10) (7.0 * f) pt
      combined = gClamp01 (veins * 0.7 + veins2 * 0.3)
      faceted = let x = gClamp01 ((combined - 0.15) / 0.7) in x * x * (3 - 2 * x)
      -- Light warp
      T2 px py = pt
      dx = gSimplex (seed + 5) (5.0 * f) pt * 0.08
      dy = gSimplex (seed + 6) (5.0 * f) pt * 0.08
      warpedPt = T2 (px + dx) (py + dy)
      wV  = gRidgedFbm 5 0.9 seed (4.0 * f) warpedPt
      wV2 = gRidgedFbm 4 0.8 (seed + 10) (7.0 * f) warpedPt
      wCombined = gClamp01 (wV * 0.7 + wV2 * 0.3)
      warped = let x = gClamp01 ((wCombined - 0.15) / 0.7) in x * x * (3 - 2 * x)
      shaped = warped ** 1.3
      col = pal shaped
  in vignetteExp 0.35 col pt

------------------------------------------------------------------------
-- Domain warp (IQ triple-pass)
------------------------------------------------------------------------

gDomainWarpStyle :: GStyle
gDomainWarpStyle seed palArr freqMul pt =
  let pal  = gApplyPalette palArr
      freq = 1.5 * freqMul
      amp  = 4.0
      T2 x y = pt
      qx  = gFbm 3 seed       freq pt
      qy  = gFbm 3 (seed + 1) freq (T2 (x + 5.2) (y + 1.3))
      rx  = gFbm 3 (seed + 2) freq (T2 (x + amp * qx + 1.7) (y + amp * qy + 9.2))
      ry  = gFbm 3 (seed + 3) freq (T2 (x + amp * qx + 8.3) (y + amp * qy + 2.8))
      val = gFbm 3 (seed + 4) freq (T2 (x + amp * rx) (y + amp * ry))
      qLen = gClamp01 (sqrt (qx * qx + qy * qy))
      baseColor = pal val
      tintColor = pal qLen
      t = gClamp01 (ry * 0.66)
  in gLerpColor t baseColor tintColor

------------------------------------------------------------------------
-- Ink (curl noise fluid)
------------------------------------------------------------------------

gInkStyle :: GStyle
gInkStyle seed palArr freqMul pt =
  let pal  = gApplyPalette palArr
      f    = freqMul
      eps  = 0.002
      -- Advection: 8 steps through curl field (unrolled)
      advStep ap =
        let T2 ax ay = ap
            curlX = (gFbm 3 seed (1.5*f) (T2 ax (ay+eps))
                   - gFbm 3 seed (1.5*f) (T2 ax (ay-eps))) / (2.0*eps)
            curlY = negate (gFbm 3 seed (1.5*f) (T2 (ax+eps) ay)
                          - gFbm 3 seed (1.5*f) (T2 (ax-eps) ay)) / (2.0*eps)
            stepSz = 0.04
        in T2 (ax + curlX * stepSz) (ay + curlY * stepSz)
      -- 8 advection steps
      a1 = advStep pt
      a2 = advStep a1
      a3 = advStep a2
      a4 = advStep a3
      a5 = advStep a4
      a6 = advStep a5
      a7 = advStep a6
      a8 = advStep a7
      flowField = gFbm 4 (seed + 10) (2.5*f) a8
      -- Flow magnitude at original point
      T2 ox oy = pt
      curlXo = (gFbm 3 seed (1.5*f) (T2 ox (oy+eps))
               - gFbm 3 seed (1.5*f) (T2 ox (oy-eps))) / (2.0*eps)
      curlYo = negate (gFbm 3 seed (1.5*f) (T2 (ox+eps) oy)
                      - gFbm 3 seed (1.5*f) (T2 (ox-eps) oy)) / (2.0*eps)
      flowMag = gClamp01 (sqrt (curlXo*curlXo + curlYo*curlYo) * 0.3)
      combined = gClamp01 (flowField * 0.7 + flowMag * 0.3)
      shaped = let sv = gClamp01 ((combined - 0.1) / 0.8) in sv * sv * (3 - 2 * sv)
      col = pal shaped
  in vignetteExp 0.25 col pt

------------------------------------------------------------------------
-- Fluted glass
------------------------------------------------------------------------

gFlutedStyle :: GStyle
gFlutedStyle seed palArr freqMul pt =
  let pal  = gApplyPalette palArr
      f    = freqMul
      T2 px py = pt
      -- Background: warped gradient
      dx = gSimplex (seed+1) (1.5*f) pt * 0.2
      dy = gSimplex (seed+2) (1.5*f) pt * 0.2
      bgWarped = (gFbm 4 seed (2.0*f) (T2 (px+dx) (py+dy))) ** 0.8
      -- Fluted glass distortion
      numRibs = 12.0 * f
      strength = 0.04
      phase = px * numRibs * 2.0 * pi
      dxFlute = cos phase * strength
      -- Re-evaluate at displaced coord
      dx2 = gSimplex (seed+1) (1.5*f) (T2 (px+dxFlute) py) * 0.2
      dy2 = gSimplex (seed+2) (1.5*f) (T2 (px+dxFlute) py) * 0.2
      bgDisplaced = (gFbm 4 seed (2.0*f) (T2 (px+dxFlute+dx2) (py+dy2))) ** 0.8
      T4 cr cg cb ca = pal bgDisplaced
      -- Fresnel + caustic
      ribPhase = sin phase
      fresnel = 0.7 + 0.3 * ribPhase * ribPhase
      caustic = (sin phase ** 8) * 0.08
      col = T4 (A.min 1 (cr * fresnel + caustic))
               (A.min 1 (cg * fresnel + caustic))
               (A.min 1 (cb * fresnel + caustic)) ca
  in vignetteExp 0.2 col pt

------------------------------------------------------------------------
-- Magma (turbulence-driven marble/lava)
------------------------------------------------------------------------

gMagmaStyle :: GStyle
gMagmaStyle seed palArr freqMul pt =
  let pal  = gApplyPalette palArr
      f    = freqMul
      T2 px py = pt
      turb = gTurbulence 6 seed (4.0*f) pt
      stripeV = sin (px * 6.0 * f * 2.0 * pi + turb * 10.0)
      -- Warp
      wdx = gSimplex (seed+5) (3.0*f) pt * 0.15
      wdy = gSimplex (seed+6) (3.0*f) pt * 0.15
      wPt = T2 (px + wdx) (py + wdy)
      turbW = gTurbulence 6 seed (4.0*f) wPt
      stripeW = sin ((px + wdx) * 6.0 * f * 2.0 * pi + turbW * 10.0)
      warped = (stripeW + 1.0) / 2.0
      shaped = warped ** 0.8
      T4 br bg bb ba = pal shaped
      -- Hot spots
      hotMask = let hv = gClamp01 ((turb - 0.55) / 0.3) in hv * hv * (3 - 2 * hv)
      T4 hr hg hb ha = pal (turb ** 0.5)
      -- Screen blend: 1 - (1-base)*(1-hot*mask)
      sr = hotMask * hr; sg = hotMask * hg; sb = hotMask * hb
  in T4 (1 - (1-br)*(1-sr)) (1 - (1-bg)*(1-sg)) (1 - (1-bb)*(1-sb)) ba

------------------------------------------------------------------------
-- Mesh gradient (Apple/iOS smooth blobs)
------------------------------------------------------------------------

gMeshStyle :: GStyle
gMeshStyle seed palArr freqMul pt =
  let pal    = gApplyPalette palArr
      T2 x y = pt
      numBlobs = A.max 3 (A.round (5.0 * freqMul) :: Exp Int)
      sigmaBase = 0.18 / freqMul
      -- Hash for blob placement
      bhash s i =
        let h  = s * 374761393 + i * 668265263
            h1 = A.xor (A.shiftR h 13) h * 1274126177
            h2 = A.xor (A.shiftR h1 16) h1
        in T2 (A.fromIntegral (h2 .&. 0xFFFF) / 65535.0 :: Exp Double)
              (A.fromIntegral (A.shiftR h2 16 .&. 0xFFFF) / 65535.0 :: Exp Double)
      -- Compute one blob's contribution: (weighted color, weight)
      blob i =
        let T2 bx _ = bhash (seed + i*3) 0
            T2 by _ = bhash (seed + i*3 + 1) 0
            T2 ci _ = bhash (seed + i*3 + 2) 0
            cx    = 0.15 + bx * 0.7
            cy    = 0.15 + by * 0.7
            sigma = sigmaBase + ci * (0.12 / freqMul)
            ddx   = x - cx; ddy = y - cy
            d2    = ddx*ddx + ddy*ddy
            w     = exp (negate d2 / (2.0 * sigma * sigma))
            T4 cr cg cb ca = pal ci
        in (w, T4 (cr*w) (cg*w) (cb*w) (ca*w))
      -- Unrolled up to 10 blobs with conditional accumulation
      addBlob i (tw, T4 tr tg tb ta) =
        let (w, T4 cr cg cb ca) = blob (A.constant i)
            tw' = tw + w
            col' = T4 (tr + cr) (tg + cg) (tb + cb) (ta + ca)
        in A.cond (A.constant i A.< numBlobs) (tw', col') (tw, T4 tr tg tb ta)
      (w0, c0) = addBlob 0 (0, T4 0 0 0 0)
      (w1, c1) = addBlob 1 (w0, c0)
      (w2, c2) = addBlob 2 (w1, c1)
      (w3, c3) = addBlob 3 (w2, c2)
      (w4, c4) = addBlob 4 (w3, c3)
      (w5, c5) = addBlob 5 (w4, c4)
      (w6, c6) = addBlob 6 (w5, c5)
      (w7, c7) = addBlob 7 (w6, c6)
      (w8, c8) = addBlob 8 (w7, c7)
      (wf, T4 fr fg fb fa) = addBlob 9 (w8, c8)
      tw = A.max wf 1e-10
      col = T4 (fr/tw) (fg/tw) (fb/tw) (fa/tw)
  in vignetteExp 0.15 col pt

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Inline vignette (avoids needing a full GColorField -> GColorField wrapper)
vignetteExp :: Exp Double -> Exp GColor -> Exp (Double, Double) -> Exp GColor
vignetteExp strength (T4 cr cg cb ca) pt =
  let T2 x y = pt
      dx = x - 0.5; dy = y - 0.5
      dist = sqrt (dx*dx + dy*dy)
      maxDist = 0.7071
      t = A.min 1.0 (dist / maxDist)
      s = gClamp01 ((t - 0.3) / 0.7)
      falloff = s * s * (3 - 2 * s)
      darkening = 1.0 - strength * falloff
  in T4 (cr * darkening) (cg * darkening) (cb * darkening) ca

------------------------------------------------------------------------
-- Registry
------------------------------------------------------------------------

gAllStyles :: [(String, GStyle)]
gAllStyles =
  [ ("crystal",    gCrystalStyle)
  , ("domainwarp", gDomainWarpStyle)
  , ("ink",        gInkStyle)
  , ("fluted",     gFlutedStyle)
  , ("magma",      gMagmaStyle)
  , ("mesh",       gMeshStyle)
  ]

gStyleByName :: String -> Maybe GStyle
gStyleByName name = lookup name gAllStyles
