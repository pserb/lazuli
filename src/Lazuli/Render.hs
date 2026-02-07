module Lazuli.Render
  ( render
  , renderToFile
  , renderToPngBytes
  ) where

import Lazuli.Types (ColorField, colorToPixel)
import Codec.Picture (Image, PixelRGBA8, generateImage, encodePng, writePng)
import qualified Data.ByteString.Lazy as BL

-- | Render a ColorField to an image. Coordinates are normalized to [0,1].
render :: Int -> Int -> ColorField -> Image PixelRGBA8
render w h field = generateImage pixelAt w h
  where
    pixelAt :: Int -> Int -> PixelRGBA8
    pixelAt x y =
      let nx = fromIntegral x / fromIntegral w :: Double
          ny = fromIntegral y / fromIntegral h :: Double
      in colorToPixel (field (nx, ny))

-- | Render and write to a PNG file.
renderToFile :: FilePath -> Int -> Int -> ColorField -> IO ()
renderToFile path w h field = writePng path (render w h field)

-- | Render to PNG bytes (for embedding or gallery use).
renderToPngBytes :: Int -> Int -> ColorField -> BL.ByteString
renderToPngBytes w h field = encodePng (render w h field)
