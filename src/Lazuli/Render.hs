{-# LANGUAGE BangPatterns #-}
module Lazuli.Render
  ( render
  , renderToFile
  , renderToPngBytes
  , renderAA
  , renderToFileAA
  , renderToPngBytesAA
  ) where

import Lazuli.Types (Color(..), ColorField, clampChannel, addColors, scaleColor)
import Codec.Picture (Image(..), PixelRGBA8(..), encodePng, writePng)
import qualified Data.ByteString.Lazy as BL
import Data.List (foldl')
import Data.Word (Word8)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import GHC.Conc (getNumCapabilities)

------------------------------------------------------------------------
-- Gamma correction LUT
------------------------------------------------------------------------

-- | Precomputed sRGB gamma lookup table (4096 entries -> Word8).
-- Avoids expensive (** 1/2.4) per channel per pixel.
gammaLUT :: VU.Vector Word8
gammaLUT = VU.generate 4096 $ \i ->
  let v = fromIntegral i / 4095.0 :: Double
      srgb | v <= 0.0031308 = v * 12.92
           | otherwise      = 1.055 * (v ** (1.0 / 2.4)) - 0.055
  in round (max 0 (min 1 srgb) * 255)
{-# NOINLINE gammaLUT #-}

-- | Fast linear-to-sRGB conversion via LUT.
linearToSRGB8 :: Double -> Word8
linearToSRGB8 !v =
  let clamped = max 0 (min 1 v)
      idx = round (clamped * 4095) :: Int
  in VU.unsafeIndex gammaLUT idx
{-# INLINE linearToSRGB8 #-}

-- | Convert Color to JuicyPixels PixelRGBA8 with sRGB gamma on RGB channels.
colorToPixel :: Color -> PixelRGBA8
colorToPixel (Color cr cg cb ca) =
  PixelRGBA8 (linearToSRGB8 cr) (linearToSRGB8 cg) (linearToSRGB8 cb) (to8 ca)
  where
    to8 v = round (clampChannel v * 255)
{-# INLINE colorToPixel #-}

------------------------------------------------------------------------
-- Parallel image generation
------------------------------------------------------------------------

-- | Generate an image in parallel by partitioning rows across capabilities.
-- Falls back to sequential for small images (< 10000 pixels).
generateImagePar :: Int -> Int -> (Int -> Int -> PixelRGBA8) -> IO (Image PixelRGBA8)
generateImagePar w h pixelFn
  | w * h < 10000 = generateImageSeq w h pixelFn
  | otherwise = do
      caps <- getNumCapabilities
      let totalBytes = w * h * 4
      mvec <- VSM.new totalBytes

      -- Partition rows into chunks
      let chunkSize = (h + caps - 1) `div` caps
          chunks = [ (startY, min h (startY + chunkSize))
                   | c <- [0 .. caps - 1]
                   , let startY = c * chunkSize
                   , startY < h
                   ]

      -- Fork one worker per chunk
      dones <- mapM (\(yStart, yEnd) -> do
        done <- newEmptyMVar
        _ <- forkIO $ do
          let go !y
                | y >= yEnd = return ()
                | otherwise = do
                    let goX !x
                          | x >= w = return ()
                          | otherwise = do
                              let PixelRGBA8 r g b a' = pixelFn x y
                                  off = (y * w + x) * 4
                              VSM.unsafeWrite mvec off       r
                              VSM.unsafeWrite mvec (off + 1) g
                              VSM.unsafeWrite mvec (off + 2) b
                              VSM.unsafeWrite mvec (off + 3) a'
                              goX (x + 1)
                    goX 0
                    go (y + 1)
          go yStart
          putMVar done ()
        return done
        ) chunks

      mapM_ takeMVar dones
      frozen <- VS.unsafeFreeze mvec
      return $ Image w h frozen

-- | Sequential fallback for small images.
generateImageSeq :: Int -> Int -> (Int -> Int -> PixelRGBA8) -> IO (Image PixelRGBA8)
generateImageSeq w h pixelFn = do
  let totalBytes = w * h * 4
  mvec <- VSM.new totalBytes
  let go !y
        | y >= h = return ()
        | otherwise = do
            let goX !x
                  | x >= w = return ()
                  | otherwise = do
                      let PixelRGBA8 r g b a' = pixelFn x y
                          off = (y * w + x) * 4
                      VSM.unsafeWrite mvec off       r
                      VSM.unsafeWrite mvec (off + 1) g
                      VSM.unsafeWrite mvec (off + 2) b
                      VSM.unsafeWrite mvec (off + 3) a'
                      goX (x + 1)
            goX 0
            go (y + 1)
  go 0
  frozen <- VS.unsafeFreeze mvec
  return $ Image w h frozen

------------------------------------------------------------------------
-- Render functions
------------------------------------------------------------------------

-- | Render a ColorField to an image. Coordinates are normalized to [0,1].
-- Single sample per pixel (no anti-aliasing).
render :: Int -> Int -> ColorField -> IO (Image PixelRGBA8)
render w h field = generateImagePar w h pixelAt
  where
    pixelAt :: Int -> Int -> PixelRGBA8
    pixelAt !x !y =
      let nx = fromIntegral x / fromIntegral w :: Double
          ny = fromIntegral y / fromIntegral h :: Double
      in colorToPixel (field (nx, ny))

-- | Render with RGSS 4-rook anti-aliasing.
-- samples=1: single center sample (no AA). samples=4: RGSS 4-rook.
renderAA :: Int -> Int -> Int -> ColorField -> IO (Image PixelRGBA8)
renderAA samples w h field
  | samples <= 1 = render w h field
  | otherwise = generateImagePar w h pixelAt
  where
    wf = fromIntegral w :: Double
    hf = fromIntegral h :: Double
    pw = 1.0 / wf  -- pixel width in normalized coords
    ph = 1.0 / hf  -- pixel height in normalized coords
    -- RGSS 4-rook offsets: no two share a row or column
    offsets = [(1/8, 5/8), (3/8, 1/8), (5/8, 7/8), (7/8, 3/8)] :: [(Double, Double)]
    n = fromIntegral (length offsets) :: Double

    pixelAt :: Int -> Int -> PixelRGBA8
    pixelAt !x !y =
      let baseX = fromIntegral x / wf
          baseY = fromIntegral y / hf
          sampleAt (ox, oy) = field (baseX + ox * pw, baseY + oy * ph)
          total = foldl' addColors (Color 0 0 0 0) (map sampleAt offsets)
      in colorToPixel (scaleColor (1.0 / n) total)

-- | Render and write to a PNG file.
renderToFile :: FilePath -> Int -> Int -> ColorField -> IO ()
renderToFile path w h field = do
  img <- render w h field
  writePng path img

-- | Render with AA and write to a PNG file.
renderToFileAA :: FilePath -> Int -> Int -> Int -> ColorField -> IO ()
renderToFileAA path samples w h field = do
  img <- renderAA samples w h field
  writePng path img

-- | Render to PNG bytes (for embedding or gallery use).
renderToPngBytes :: Int -> Int -> ColorField -> IO BL.ByteString
renderToPngBytes w h field = do
  img <- render w h field
  return $ encodePng img

-- | Render with AA to PNG bytes.
renderToPngBytesAA :: Int -> Int -> Int -> ColorField -> IO BL.ByteString
renderToPngBytesAA samples w h field = do
  img <- renderAA samples w h field
  return $ encodePng img
