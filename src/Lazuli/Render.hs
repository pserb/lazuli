module Lazuli.Render
  ( render
  , renderToFile
  , renderToPngBytes
  , renderAA
  , renderToFileAA
  , renderToPngBytesAA
  ) where

import Lazuli.Types (Color(..), ColorField, clampChannel, addColors, scaleColor)
import Codec.Picture (Image, PixelRGBA8(..), generateImage, encodePng, writePng)
import qualified Data.ByteString.Lazy as BL
import Data.List (foldl')

-- | sRGB gamma: linear [0,1] to sRGB [0,1].
-- Makes midtones richer, output more photographic.
linearToSRGB :: Double -> Double
linearToSRGB v
  | v <= 0.0031308 = v * 12.92
  | otherwise       = 1.055 * (v ** (1.0 / 2.4)) - 0.055

-- | Convert Color to JuicyPixels PixelRGBA8 with sRGB gamma on RGB channels.
colorToPixel :: Color -> PixelRGBA8
colorToPixel (Color cr cg cb ca) =
  PixelRGBA8 (to8 $ linearToSRGB cr) (to8 $ linearToSRGB cg) (to8 $ linearToSRGB cb) (to8 ca)
  where
    to8 v = round (clampChannel v * 255)

-- | Render a ColorField to an image. Coordinates are normalized to [0,1].
-- Single sample per pixel (no anti-aliasing).
render :: Int -> Int -> ColorField -> Image PixelRGBA8
render w h field = generateImage pixelAt w h
  where
    pixelAt :: Int -> Int -> PixelRGBA8
    pixelAt x y =
      let nx = fromIntegral x / fromIntegral w :: Double
          ny = fromIntegral y / fromIntegral h :: Double
      in colorToPixel (field (nx, ny))

-- | Render with RGSS 4-rook anti-aliasing.
-- samples=1: single center sample (no AA). samples=4: RGSS 4-rook.
renderAA :: Int -> Int -> Int -> ColorField -> Image PixelRGBA8
renderAA samples w h field
  | samples <= 1 = render w h field
  | otherwise = generateImage pixelAt w h
  where
    wf = fromIntegral w :: Double
    hf = fromIntegral h :: Double
    pw = 1.0 / wf  -- pixel width in normalized coords
    ph = 1.0 / hf  -- pixel height in normalized coords
    -- RGSS 4-rook offsets: no two share a row or column
    offsets = [(1/8, 5/8), (3/8, 1/8), (5/8, 7/8), (7/8, 3/8)] :: [(Double, Double)]
    n = fromIntegral (length offsets) :: Double

    pixelAt :: Int -> Int -> PixelRGBA8
    pixelAt x y =
      let baseX = fromIntegral x / wf
          baseY = fromIntegral y / hf
          sampleAt (ox, oy) = field (baseX + ox * pw, baseY + oy * ph)
          total = foldl' addColors (Color 0 0 0 0) (map sampleAt offsets)
      in colorToPixel (scaleColor (1.0 / n) total)

-- | Render and write to a PNG file.
renderToFile :: FilePath -> Int -> Int -> ColorField -> IO ()
renderToFile path w h field = writePng path (render w h field)

-- | Render with AA and write to a PNG file.
renderToFileAA :: FilePath -> Int -> Int -> Int -> ColorField -> IO ()
renderToFileAA path samples w h field = writePng path (renderAA samples w h field)

-- | Render to PNG bytes (for embedding or gallery use).
renderToPngBytes :: Int -> Int -> ColorField -> BL.ByteString
renderToPngBytes w h field = encodePng (render w h field)

-- | Render with AA to PNG bytes.
renderToPngBytesAA :: Int -> Int -> Int -> ColorField -> BL.ByteString
renderToPngBytesAA samples w h field = encodePng (renderAA samples w h field)
