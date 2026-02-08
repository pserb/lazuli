{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators    #-}
-- | GPU-accelerated rendering using the @accelerate@ library.
--
-- The public API mirrors Lazuli.Render but executes the entire
-- field-evaluation pipeline on the GPU.  Post-processing effects
-- are still applied on the CPU after pixel readback.
--
-- Build with @cabal build -f gpu@ to enable.  The @accelerate-llvm-ptx@
-- backend targets NVIDIA GPUs via CUDA; @accelerate-llvm-native@ provides
-- a multi-core CPU fallback that still benefits from accelerate's
-- optimising compiler.
module Lazuli.GPU
  ( gpuRender
  , gpuRenderToFile
  , gpuAvailable
  ) where

import Prelude as P

import Data.Array.Accelerate                as A
import Data.Array.Accelerate.LLVM.PTX       as PTX  -- NVIDIA GPU backend
-- For systems without CUDA, swap the import above for:
-- import Data.Array.Accelerate.LLVM.Native as Native

import Lazuli.Types (Color(..), Palette)
import Lazuli.Effect (Effect, applyEffects)
import Lazuli.GPU.Field (GColor, gApplyPalette)
import Lazuli.GPU.Style (gStyleByName, GStyle)

import Codec.Picture (Image(..), PixelRGBA8(..), writePng)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word (Word8)
import Control.Exception (try, SomeException)

------------------------------------------------------------------------
-- GPU availability check
------------------------------------------------------------------------

-- | Check whether the GPU backend is functional.
-- Attempts a trivial accelerate computation; returns False on failure.
gpuAvailable :: IO Bool
gpuAvailable = do
  result <- try $ do
    let arr  = A.use (A.fromList (Z :. (1 :: Int)) [42 :: Int])
        prog = A.map (+ 1) arr
    let !_ = PTX.run prog
    return True
  case result of
    Right b                -> return b
    Left (_ :: SomeException) -> return False

------------------------------------------------------------------------
-- Palette upload
------------------------------------------------------------------------

-- | Sample the Haskell palette function at 256 points and upload to
-- an accelerate array for GPU-side interpolation.
uploadPalette :: Palette -> Acc (A.Vector GColor)
uploadPalette pal =
  let n = 256 :: Int
      samples = [ let t = P.fromIntegral i / P.fromIntegral (n P.- 1) :: Double
                      Color cr cg cb ca = pal t
                  in (cr, cg, cb, ca)
                | i <- [0 .. n P.- 1]
                ]
  in A.use (A.fromList (Z :. n) samples)

------------------------------------------------------------------------
-- Core rendering
------------------------------------------------------------------------

-- | Render a style to a JuicyPixels image using the GPU.
gpuRender :: [Effect]     -- ^ Post-processing effects (applied on CPU)
          -> String       -- ^ Style name
          -> Int          -- ^ Seed
          -> Int          -- ^ Width
          -> Int          -- ^ Height
          -> Palette      -- ^ Palette function
          -> Int          -- ^ AA samples (1 = off, 4 = RGSS)
          -> Double       -- ^ Frequency multiplier
          -> IO (Image PixelRGBA8)
gpuRender effects styleName seed w h palette samples freqMul = do
  let styleFn = case gStyleByName styleName of
        Just s  -> s
        Nothing -> error $ "GPU: unknown style: " ++ styleName

  -- Build the accelerate program
  let palArr  = uploadPalette palette
      seedE   = A.constant seed
      freqE   = A.constant freqMul

      -- Generate pixel coordinate grid
      sh      = A.constant (Z :. h :. w) :: Exp DIM2
      wf      = A.fromIntegral (A.constant w) :: Exp Double
      hf      = A.fromIntegral (A.constant h) :: Exp Double

      -- For each pixel, evaluate the style color field
      pixels  = A.generate sh $ \ix ->
        let Z :. yi :. xi = A.unlift ix :: Z :. Exp Int :. Exp Int
        in if samples P.>= 4
           then gpuPixelAA styleFn palArr seedE freqE wf hf xi yi
           else gpuPixelNoAA styleFn palArr seedE freqE wf hf xi yi

      -- Apply sRGB gamma correction on GPU
      gammaPixels = A.map linearToSRGB8 pixels

  -- Execute on GPU
  let result = PTX.run gammaPixels :: A.Array DIM2 (Word8, Word8, Word8, Word8)

  -- Convert to JuicyPixels Image
  img <- arrayToImage w h result

  -- Apply post-processing effects (CPU)
  return $ applyEffects effects img

-- | Single-sample pixel evaluation (no AA).
gpuPixelNoAA :: GStyle
             -> Acc (A.Vector GColor)
             -> Exp Int -> Exp Double
             -> Exp Double -> Exp Double
             -> Exp Int -> Exp Int
             -> Exp GColor
gpuPixelNoAA style palArr seed freq wf hf xi yi =
  let nx = A.fromIntegral xi / wf
      ny = A.fromIntegral yi / hf
  in style seed palArr freq (A.T2 nx ny)

-- | 4-sample RGSS anti-aliased pixel evaluation.
gpuPixelAA :: GStyle
           -> Acc (A.Vector GColor)
           -> Exp Int -> Exp Double
           -> Exp Double -> Exp Double
           -> Exp Int -> Exp Int
           -> Exp GColor
gpuPixelAA style palArr seed freq wf hf xi yi =
  let baseX = A.fromIntegral xi / wf
      baseY = A.fromIntegral yi / hf
      pw    = 1.0 / wf
      ph    = 1.0 / hf
      -- RGSS 4-rook offsets
      s1 = style seed palArr freq (A.T2 (baseX + (1.0/8.0) * pw) (baseY + (5.0/8.0) * ph))
      s2 = style seed palArr freq (A.T2 (baseX + (3.0/8.0) * pw) (baseY + (1.0/8.0) * ph))
      s3 = style seed palArr freq (A.T2 (baseX + (5.0/8.0) * pw) (baseY + (7.0/8.0) * ph))
      s4 = style seed palArr freq (A.T2 (baseX + (7.0/8.0) * pw) (baseY + (3.0/8.0) * ph))
      -- Average
      T4 r1 g1 b1 a1 = s1
      T4 r2 g2 b2 a2 = s2
      T4 r3 g3 b3 a3 = s3
      T4 r4 g4 b4 a4 = s4
  in T4 ((r1+r2+r3+r4) * 0.25) ((g1+g2+g3+g4) * 0.25)
        ((b1+b2+b3+b4) * 0.25) ((a1+a2+a3+a4) * 0.25)

------------------------------------------------------------------------
-- sRGB gamma (GPU-side)
------------------------------------------------------------------------

-- | Convert a linear-space GColor to sRGB Word8 RGBA on the GPU.
linearToSRGB8 :: Exp GColor -> Exp (Word8, Word8, Word8, Word8)
linearToSRGB8 (T4 cr cg cb ca) =
  let gamma v =
        let clamped = A.max 0 (A.min 1 v)
            lo = clamped * 12.92
            hi = 1.055 * (clamped ** (1.0/2.4)) - 0.055
            srgb = A.cond (clamped A.<= 0.0031308) lo hi
        in A.round (srgb * 255.0) :: Exp Word8
      alpha = A.round (A.max 0 (A.min 1 ca) * 255.0) :: Exp Word8
  in T4 (gamma cr) (gamma cg) (gamma cb) alpha

------------------------------------------------------------------------
-- Array -> Image conversion
------------------------------------------------------------------------

-- | Convert an accelerate 2D array of RGBA Word8 tuples to a JuicyPixels image.
arrayToImage :: Int -> Int -> A.Array DIM2 (Word8, Word8, Word8, Word8) -> IO (Image PixelRGBA8)
arrayToImage w h arr = do
  let totalBytes = w P.* h P.* 4
  mvec <- VSM.new totalBytes
  let go y
        | y P.>= h  = return ()
        | otherwise = do
            let goX x
                  | x P.>= w  = return ()
                  | otherwise = do
                      let (r, g, b, a') = A.indexArray arr (Z :. y :. x)
                          off = (y P.* w P.+ x) P.* 4
                      VSM.unsafeWrite mvec off       r
                      VSM.unsafeWrite mvec (off P.+ 1) g
                      VSM.unsafeWrite mvec (off P.+ 2) b
                      VSM.unsafeWrite mvec (off P.+ 3) a'
                      goX (x P.+ 1)
            goX 0
            go (y P.+ 1)
  go 0
  frozen <- VS.unsafeFreeze mvec
  return $ Image w h frozen

------------------------------------------------------------------------
-- File output
------------------------------------------------------------------------

-- | Render with GPU and write directly to a PNG file.
gpuRenderToFile :: [Effect] -> FilePath -> String -> Int -> Int
                -> Palette -> Int -> Double -> IO ()
gpuRenderToFile effects path styleName seed w h palette samples freq = do
  img <- gpuRender effects styleName seed w h palette samples freq
  writePng path img
