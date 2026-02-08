{-# LANGUAGE BangPatterns #-}
module Lazuli.Effect
  ( Effect
  , applyEffects
  , parseEffect
  , randomEffect
  , randomEffects
  , effectDescriptions
  ) where

import Codec.Picture
import Codec.Picture.Types
import Data.List (foldl')
import Data.Word (Word8)
import qualified Data.Vector.Storable as VS
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Random (StdGen, mkStdGen, randomR)
import Lazuli.Noise (simplex)
import Text.Printf (printf)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

type Effect = Image PixelRGBA8 -> Image PixelRGBA8

applyEffects :: [Effect] -> Image PixelRGBA8 -> Image PixelRGBA8
applyEffects effects img = foldl' (flip id) img effects

------------------------------------------------------------------------
-- Descriptions
------------------------------------------------------------------------

effectDescriptions :: [(String, String)]
effectDescriptions =
  [ ("blur:radius", "Gaussian blur with given radius")
  , ("sharpen:amount", "Sharpen image by amount")
  , ("bloom:threshold:intensity:radius", "Bloom effect on bright areas")
  , ("vignette:radius:softness", "Radial vignette")
  , ("contrast:factor", "Adjust contrast (1.0 = normal)")
  , ("saturation:factor", "Adjust saturation (1.0 = normal)")
  , ("brightness:offset", "Adjust brightness (-1.0 to 1.0)")
  , ("sepia:amount", "Sepia tone amount (0.0 to 1.0)")
  , ("grayscale:method", "Grayscale (method: avg, luma, desat)")
  , ("pixelate:size", "Pixelate with block size")
  , ("dither:amount", "Ordered dithering")
  , ("halftone:size:freq", "Newprint halftone dots")
  , ("noise:amount:size", "Add random noise")
  , ("filmGrain:amount:size:colorAmount", "Physically-based film grain (luminance-dependent)")
  , ("distort:amount:center", "Radial distortion")
  , ("flutedGlass:amount:width", "Vertical fluted glass effect")
  , ("water:amp:freq", "Water wave distortion")
  , ("paperTexture:int:scale", "Paper grain texture")
  , ("crackle:amount", "Crackle/fracture effect")
  , ("marble:intensity:scale", "Marble veining")
  , ("wood:intensity:width", "Wood grain pattern")
  , ("plastic:spec:r:g:b", "Glossy plastic with tint")
  , ("metal:intensity:direction", "Brushed metal look")
  , ("glass:refraction", "Glass refraction effect")
  , ("oilPaint:radius:levels", "Oil painting effect")
  , ("charcoal:intensity", "Charcoal sketch effect")
  , ("neon:threshold:r:g:b", "Neon glow effect")
  , ("ink:intensity", "Ink illustration effect")
  ]

------------------------------------------------------------------------
-- Random Effects
------------------------------------------------------------------------

-- | Generate a random effect and its string representation.
randomEffect :: StdGen -> (Effect, String, StdGen)
randomEffect gen =
  let (idx, g1) = randomR (0, length effectPool - 1) gen
      (name, genFn) = effectPool !! idx
  in genFn g1 name

type EffectGen = StdGen -> String -> (Effect, String, StdGen)

effectPool :: [(String, EffectGen)]
effectPool =
  [ ("blur", gen1 1 5 gaussianBlur)
  , ("sharpen", gen1 0.5 2 sharpen)
  , ("bloom", gen3 (0.5, 0.9) (0.2, 0.8) (1, 10) bloom)
  , ("vignette", gen2 (0.3, 0.7) (0.1, 0.5) vignette)
  , ("contrast", gen1 1.0 2.0 contrast)
  , ("saturation", gen1 0.0 2.0 saturation)
  , ("brightness", gen1 (-0.2) 0.2 brightness)
  , ("sepia", gen1 0.0 1.0 sepia)
  , ("grayscale", \g n -> (grayscale "luma", n ++ ":luma", g))
  , ("pixelate", gen1 2 16 pixelate)
  , ("dither", gen1 0.05 0.2 dither)
  , ("halftone", gen2 (2.0, 5.0) (5.0, 20.0) halftone)
  , ("noise", gen2 (0.01, 0.1) (1, 4) noise)
  , ("filmGrain", gen3 (0.3, 0.8) (1.0, 3.0) (0.0, 0.5) filmGrain)
  , ("distort", gen2 (0.1, 0.5) (1, 10) distort)
  , ("flutedGlass", gen2 (0.1, 0.5) (5, 20) flutedGlass)
  , ("water", gen2 (0.01, 0.05) (0.1, 0.5) water)
  , ("paperTexture", gen2 (0.1, 0.4) (1, 5) paperTexture)
  , ("crackle", gen1 0.1 0.5 crackle)
  , ("marble", gen2 (0.1, 0.8) (0.01, 0.2) marble)
  , ("wood", gen2 (0.1, 0.5) (5, 20) wood)
  , ("plastic", gen4 (0.1, 0.8) (0, 255) (0, 255) (0, 255) plastic)
  , ("metal", \g n ->
       let (i, g1) = randomR (0.1, 0.5) g
           (d, g2) = randomR (0, 2 :: Int) g1
           dir = case d of 0 -> "horizontal"; 1 -> "vertical"; _ -> "diagonal"
       in (metal i dir, printf "%s:%.2f:%s" n i dir, g2))
  , ("glass", gen1 0.1 1.0 glass)
  , ("oilPaint", \g n -> 
      let (r, g1) = randomR (1, 5) g
          (l, g2) = randomR (4, 12) g1
      in (oilPaint r l, printf "%s:%d:%d" n r l, g2))
  , ("charcoal", gen1 0.5 2.0 charcoal)
  , ("neon", gen4 (0.3, 0.7) (0, 255) (0, 255) (0, 255) neon)
  , ("ink", gen1 0.5 0.9 ink)
  ]

gen1 :: Double -> Double -> (Double -> Effect) -> EffectGen
gen1 minV maxV f g n =
  let (v, g1) = randomR (minV, maxV) g
  in (f v, printf "%s:%.2f" n v, g1)

gen2 :: (Double, Double) -> (Double, Double) -> (Double -> Double -> Effect) -> EffectGen
gen2 (min1, max1) (min2, max2) f g n =
  let (v1, g1) = randomR (min1, max1) g
      (v2, g2) = randomR (min2, max2) g1
  in (f v1 v2, printf "%s:%.2f:%.2f" n v1 v2, g2)

gen3 :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double -> Double -> Double -> Effect) -> EffectGen
gen3 (min1, max1) (min2, max2) (min3, max3) f g n =
  let (v1, g1) = randomR (min1, max1) g
      (v2, g2) = randomR (min2, max2) g1
      (v3, g3) = randomR (min3, max3) g2
  in (f v1 v2 v3, printf "%s:%.2f:%.2f:%.2f" n v1 v2 v3, g3)

gen4 :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double -> Double -> Double -> Double -> Effect) -> EffectGen
gen4 (min1, max1) (min2, max2) (min3, max3) (min4, max4) f g n =
  let (v1, g1) = randomR (min1, max1) g
      (v2, g2) = randomR (min2, max2) g1
      (v3, g3) = randomR (min3, max3) g2
      (v4, g4) = randomR (min4, max4) g3
  in (f v1 v2 v3 v4, printf "%s:%.2f:%.2f:%.2f:%.2f" n v1 v2 v3 v4, g4)

-- | Generate a random number of random effects.
randomEffects :: StdGen -> ([Effect], [String], StdGen)
randomEffects gen =
  let (num, g1) = randomR (0, 3 :: Int) gen -- 0 to 3 effects
      go 0 g accE accS = (accE, accS, g)
      go n g accE accS =
        let (e, s, g') = randomEffect g
        in go (n - 1) g' (e : accE) (s : accS)
  in go num g1 [] []

------------------------------------------------------------------------
-- Parser
------------------------------------------------------------------------

parseEffect :: String -> Either String Effect
parseEffect str = case break (== ':') str of
  (name, rest) -> case name of
    "blur"         -> parse1 gaussianBlur rest
    "sharpen"      -> parse1 sharpen rest
    "bloom"        -> parse3 bloom rest
    "vignette"     -> parse2 vignette rest
    "contrast"     -> parse1 contrast rest
    "saturation"   -> parse1 saturation rest
    "brightness"   -> parse1 brightness rest
    "sepia"        -> parse1 sepia rest
    "grayscale"    -> parseStr grayscale rest
    "pixelate"     -> parse1 pixelate rest
    "dither"       -> parse1 dither rest
    "noise"        -> parse2 noise rest
    "filmGrain"    -> parse3 filmGrain rest
    "distort"      -> parse2 distort rest
    "flutedGlass"  -> parse2 flutedGlass rest
    "halftone"     -> parse2 halftone rest
    "water"        -> parse2 water rest
    "paperTexture" -> parse2 paperTexture rest
    "crackle"      -> parse1 crackle rest
    "marble"       -> parse2 marble rest
    "wood"         -> parse2 wood rest
    "plastic"      -> parse4 plastic rest
    "metal"        -> parse2Str metal rest
    "glass"        -> parse1 glass rest
    "oilPaint"     -> parse2Int oilPaint rest
    "charcoal"     -> parse1 charcoal rest
    "neon"         -> parse4 neon rest
    "ink"          -> parse1 ink rest
    _              -> Left $ "Unknown effect: " ++ name

  where
    parse1 f r = case parseArgs r of
      [a] -> Right (f a)
      _   -> Left "Expected 1 numeric argument"

    parse2 f r = case parseArgs r of
      [a, b] -> Right (f a b)
      _      -> Left "Expected 2 numeric arguments"

    parse3 f r = case parseArgs r of
      [a, b, c] -> Right (f a b c)
      _         -> Left "Expected 3 numeric arguments"

    parse4 f r = case parseArgs r of
      [a, b, c, d] -> Right (f a b c d)
      _            -> Left "Expected 4 numeric arguments"

    parseStr f r = case parseArgsStr r of
      [s] -> Right (f s)
      _   -> Left "Expected 1 string argument"

    parse2Str f r = case parseArgsStr r of
      [n, s] -> case readMaybe n of
         Just v -> Right (f v s)
         Nothing -> Left "Expected number then string"
      _ -> Left "Expected 2 arguments (number, string)"

    parse2Int f r = case parseArgs r of
      [a, b] -> Right (f (round a) (round b))
      _      -> Left "Expected 2 numeric arguments"

    parseArgs :: String -> [Double]
    parseArgs s = mapMaybe readMaybe (splitArgs s)

    parseArgsStr :: String -> [String]
    parseArgsStr s = splitArgs s

    splitArgs [] = []
    splitArgs (':':cs) = case break (== ':') cs of
      (w, rest) -> w : splitArgs rest
    splitArgs _ = []

------------------------------------------------------------------------
-- Implementations
------------------------------------------------------------------------

clamp :: Double -> Double
clamp x = max 0 (min 255 x)
{-# INLINE clamp #-}

clampUnit :: Double -> Double
clampUnit x = max 0 (min 1 x)
{-# INLINE clampUnit #-}

-- | Helper to map pixels
mapPixels :: (PixelRGBA8 -> PixelRGBA8) -> Image PixelRGBA8 -> Image PixelRGBA8
mapPixels f = pixelMap f

-- | Gaussian Blur
gaussianBlur :: Double -> Effect
gaussianBlur radius img
  | radius <= 0 = img
  | otherwise   =
      let sigma = radius / 2
          size = ceiling radius * 2 + 1
          kernel = generateGaussianKernel size sigma
      in convolve kernel img

generateGaussianKernel :: Int -> Double -> Image PixelF
generateGaussianKernel size sigma = generateImage gen size size
  where
    center = fromIntegral size / 2
    c2 = 2 * sigma * sigma
    gen x y =
      let dx = fromIntegral x - center
          dy = fromIntegral y - center
          val = exp (-(dx*dx + dy*dy) / c2)
      in realToFrac val

normalizeKernel :: Image PixelF -> Image PixelF
normalizeKernel img@(Image w h _) =
  let sumK = foldl' (\acc y -> foldl' (\a x -> a + val x y) acc [0..w-1]) 0 [0..h-1]
      val x y = pixelAt img x y
      norm = 1 / sumK
  in pixelMap (\v -> v * norm) img

convolve :: Image PixelF -> Image PixelRGBA8 -> Image PixelRGBA8
convolve k img =
  let k' = normalizeKernel k
      (Image kw kh _) = k'
      (Image w h _) = img
      halfW = kw `div` 2
      halfH = kh `div` 2
  in generateImage (\x y -> computePixel x y k' halfW halfH w h img) w h

computePixel :: Int -> Int -> Image PixelF -> Int -> Int -> Int -> Int -> Image PixelRGBA8 -> PixelRGBA8
computePixel x y kernel hw hh w h src =
  let (Image kw kh _) = kernel
      foldFn (ar, ag, ab, aa) kx ky =
        let sx = x + kx - hw
            sy = y + ky - hh
        in if sx < 0 || sx >= w || sy < 0 || sy >= h
           then (ar, ag, ab, aa)
           else
             let PixelRGBA8 r g b a = pixelAt src sx sy
                 wgt = pixelAt kernel kx ky
                 wgt' = realToFrac wgt
             in (ar + fromIntegral r * wgt',
                 ag + fromIntegral g * wgt',
                 ab + fromIntegral b * wgt',
                 aa + fromIntegral a * wgt')
      (r', g', b', a') = foldl' (\acc ky -> foldl' (\acc' kx -> foldFn acc' kx ky) acc [0..kw-1]) (0,0,0,0) [0..kh-1]
  in PixelRGBA8 (round $ clamp r') (round $ clamp g') (round $ clamp b') (round $ clamp a')

-- | Sharpen
sharpen :: Double -> Effect
sharpen amount img =
  let blurred = gaussianBlur 1.0 img
      (Image w h _) = img
  in generateImage (\x y ->
       let PixelRGBA8 r1 g1 b1 a1 = pixelAt img x y
           PixelRGBA8 r2 g2 b2 a2 = pixelAt blurred x y
           f v1 v2 = clamp $ fromIntegral v1 + amount * (fromIntegral v1 - fromIntegral v2)
       in PixelRGBA8 (round $ f r1 r2) (round $ f g1 g2) (round $ f b1 b2) a1
     ) w h

-- | Bloom
bloom :: Double -> Double -> Double -> Effect
bloom threshold intensity radius img =
  let (Image w h _) = img
      brightPass = generateImage (\x y ->
        let PixelRGBA8 r g b a = pixelAt img x y
            lum = 0.2126 * fromIntegral r + 0.7152 * fromIntegral g + 0.0722 * fromIntegral b
        in if lum > threshold * 255
           then PixelRGBA8 r g b a
           else PixelRGBA8 0 0 0 255
        ) w h
      blurred = gaussianBlur radius brightPass
  in generateImage (\x y ->
       let PixelRGBA8 r1 g1 b1 a1 = pixelAt img x y
           PixelRGBA8 r2 g2 b2 _ = pixelAt blurred x y
           add v1 v2 = clamp $ fromIntegral v1 + intensity * fromIntegral v2
       in PixelRGBA8 (round $ add r1 r2) (round $ add g1 g2) (round $ add b1 b2) a1
     ) w h

-- | Vignette
vignette :: Double -> Double -> Effect
vignette radius softness img =
  let (Image w h _) = img
      cx = fromIntegral w / 2 :: Double
      cy = fromIntegral h / 2 :: Double
      maxDist = sqrt (cx*cx + cy*cy)
      rNorm = radius * maxDist
  in generateImage (\x y ->
       let dx = fromIntegral x - cx
           dy = fromIntegral y - cy
           dist = sqrt (dx*dx + dy*dy)
           factor = 1.0 - smoothStep rNorm (rNorm + softness * maxDist) dist
           PixelRGBA8 r g b a = pixelAt img x y
           mult v = round $ clamp $ fromIntegral v * factor
       in PixelRGBA8 (mult r) (mult g) (mult b) a
     ) w h

smoothStep :: Double -> Double -> Double -> Double
smoothStep edge0 edge1 x =
  let t = clampUnit ((x - edge0) / (edge1 - edge0))
  in t * t * (3.0 - 2.0 * t)

-- | Contrast
contrast :: Double -> Effect
contrast factor = mapPixels $ \(PixelRGBA8 r g b a) ->
  let f v = clamp $ ((fromIntegral v / 255.0 - 0.5) * factor + 0.5) * 255.0
  in PixelRGBA8 (round $ f r) (round $ f g) (round $ f b) a

-- | Saturation
saturation :: Double -> Effect
saturation factor = mapPixels $ \(PixelRGBA8 r g b a) ->
  let r' = fromIntegral r
      g' = fromIntegral g
      b' = fromIntegral b
      lum = 0.2126 * r' + 0.7152 * g' + 0.0722 * b'
      sat v = clamp $ lum + (v - lum) * factor
  in PixelRGBA8 (round $ sat r') (round $ sat g') (round $ sat b') a

-- | Brightness
brightness :: Double -> Effect
brightness offset = mapPixels $ \(PixelRGBA8 r g b a) ->
  let off = offset * 255
      f v = clamp $ fromIntegral v + off
  in PixelRGBA8 (round $ f r) (round $ f g) (round $ f b) a

-- | Sepia
sepia :: Double -> Effect
sepia amount = mapPixels $ \(PixelRGBA8 r g b a) ->
  let r' = fromIntegral r
      g' = fromIntegral g
      b' = fromIntegral b
      tr = 0.393 * r' + 0.769 * g' + 0.189 * b'
      tg = 0.349 * r' + 0.686 * g' + 0.168 * b'
      tb = 0.272 * r' + 0.534 * g' + 0.131 * b'
      mix v1 v2 = clamp $ v1 * (1 - amount) + v2 * amount
  in PixelRGBA8 (round $ mix r' tr) (round $ mix g' tg) (round $ mix b' tb) a

-- | Grayscale
grayscale :: String -> Effect
grayscale method = mapPixels $ \(PixelRGBA8 r g b a) ->
  let r' = fromIntegral r
      g' = fromIntegral g
      b' = fromIntegral b
      v = case method of
            "luma"  -> 0.2126 * r' + 0.7152 * g' + 0.0722 * b'
            "desat" -> (max r' (max g' b') + min r' (min g' b')) / 2
            _       -> (r' + g' + b') / 3 -- avg
      v8 = round $ clamp v
  in PixelRGBA8 v8 v8 v8 a

-- | Pixelate
pixelate :: Double -> Effect
pixelate blockSize img
  | blockSize <= 1 = img
  | otherwise =
      let (Image w h _) = img
          bs = max 1 (round blockSize)
      in generateImage (\x y ->
           let px = (x `div` bs) * bs
               py = (y `div` bs) * bs
           in pixelAt img (min (w-1) px) (min (h-1) py)
         ) w h

-- | Dither
dither :: Double -> Effect
dither amount img =
  let (Image w h _) = img
  in generateImage (\x y ->
       let PixelRGBA8 r g b a = pixelAt img x y
           -- Simple Bayer 2x2 matrix
           bayer = [[0, 2], [3, 1]]
           bx = x `mod` 2
           by = y `mod` 2
           threshold = (bayer !! by !! bx) / 4.0 - 0.5
           d = threshold * amount * 255
           f v = clamp $ fromIntegral v + d
       in PixelRGBA8 (round $ f r) (round $ f g) (round $ f b) a
     ) w h

-- | Noise (Simplex)
noise :: Double -> Double -> Effect
noise amount size img =
  let (Image w h _) = img
      sf = simplex 12345 (1.0 / size)
  in generateImage (\x y ->
       let PixelRGBA8 r g b a = pixelAt img x y
           nVal = sf (fromIntegral x, fromIntegral y) -- returns 0..1
           -- Map 0..1 to -1..1
           n = (nVal * 2.0 - 1.0)
           f v = clamp $ fromIntegral v + n * amount * 255
       in PixelRGBA8 (round $ f r) (round $ f g) (round $ f b) a
     ) w h

-- | Film Grain (physically-based, luminance-dependent)
filmGrain :: Double -> Double -> Double -> Effect
filmGrain amount size colorAmount img =
  let (Image w h _) = img
      freq = 1.0 / size
      sfBase    = simplex 12345 freq
      sfColorR  = simplex 12346 freq
      sfColorG  = simplex 12347 freq
      sfColorB  = simplex 12348 freq
      sfCluster = simplex 12445 0.003  -- low-freq cluster modulation
  in generateImage (\x y ->
       let PixelRGBA8 r g b a = pixelAt img x y
           -- Pixel luminance
           !lum = 0.2126 * fromIntegral r + 0.7152 * fromIntegral g + 0.0722 * fromIntegral b
           -- Luminance-dependent grain strength: shadows grainier than highlights
           !grainStrength = amount * 25.0 * (1.0 - 0.7 * ((lum / 255.0) ** 0.8))
           -- Grain clustering: spatial variation in grain density
           !cluster = 0.5 + simplex2dAt sfCluster x y * 0.5
           !strength = grainStrength * cluster
           -- Base grain [-1, 1]
           !baseG = simplex2dAt sfBase x y * 2.0 - 1.0
           -- Per-channel color grain
           !grainR = baseG + colorAmount * (simplex2dAt sfColorR x y * 2.0 - 1.0) * 0.3
           !grainGr = baseG + colorAmount * (simplex2dAt sfColorG x y * 2.0 - 1.0) * 0.3
           !grainBl = baseG + colorAmount * (simplex2dAt sfColorB x y * 2.0 - 1.0) * 0.3
       in PixelRGBA8 (round $ clamp $ fromIntegral r + grainR * strength)
                     (round $ clamp $ fromIntegral g + grainGr * strength)
                     (round $ clamp $ fromIntegral b + grainBl * strength)
                     a
     ) w h
  where
    simplex2dAt sf px py = sf (fromIntegral px, fromIntegral py)

-- | Distort
distort :: Double -> Double -> Effect
distort amount center img =
  let (Image w h _) = img
      cx = fromIntegral w / 2 :: Double
      cy = fromIntegral h / 2 :: Double
  in generateImage (\x y ->
       let dx = fromIntegral x - cx
           dy = fromIntegral y - cy
           r = sqrt (dx*dx + dy*dy)
           theta = atan2 dy dx
           r' = r * (1 + amount * sin (r * center / 100.0))
           sx = cx + r' * cos theta
           sy = cy + r' * sin theta
       in sampleBilinear img sx sy
     ) w h

-- | Fluted Glass
flutedGlass :: Double -> Double -> Effect
flutedGlass amount ridgeWidth img =
  let (Image w h _) = img
  in generateImage (\x y ->
       let dx = amount * sin (fromIntegral x / ridgeWidth * pi * 2)
           sx = fromIntegral x + dx
           sy = fromIntegral y
       in sampleBilinear img sx sy
     ) w h

-- | Halftone
halftone :: Double -> Double -> Effect
halftone dotSize freq img =
  let (Image w h _) = img
      frequency = if freq <= 0 then 10 else freq
  in generateImage (\x y ->
       let gridX = fromIntegral x / frequency
           gridY = fromIntegral y / frequency
           cx = (fromIntegral (floor gridX) + 0.5) * frequency
           cy = (fromIntegral (floor gridY) + 0.5) * frequency
           dx = fromIntegral x - cx
           dy = fromIntegral y - cy
           dist = sqrt (dx*dx + dy*dy)
           
           PixelRGBA8 r g b _ = sampleBilinear img cx cy
           lum = (fromIntegral r + fromIntegral g + fromIntegral b) / (3 * 255.0)
           maxR = dotSize * (1.0 - lum)
       in if dist < maxR
          then PixelRGBA8 0 0 0 255
          else PixelRGBA8 255 255 255 255
     ) w h

-- | Water
water :: Double -> Double -> Effect
water amplitude frequency img =
  let (Image w h _) = img
  in generateImage (\x y ->
       let dx = amplitude * 100 * sin (fromIntegral y * frequency / 10 + fromIntegral x * frequency / 20)
           dy = amplitude * 100 * cos (fromIntegral x * frequency / 10 + fromIntegral y * frequency / 20)
       in sampleBilinear img (fromIntegral x + dx) (fromIntegral y + dy)
     ) w h

-- | Paper Texture (multi-octave FBM, luminance-aware)
paperTexture :: Double -> Double -> Effect
paperTexture intensity scale img =
  let (Image w h _) = img
      -- 3-octave FBM for fine fiber detail
      sf1 = simplex 54321 (1.0 / scale)
      sf2 = simplex 54322 (2.0 / scale)
      sf3 = simplex 54323 (4.0 / scale)
      -- Low-frequency surface undulation
      sfLarge = simplex 54400 (0.1 / scale)
  in generateImage (\x y ->
       let PixelRGBA8 r g b a = pixelAt img x y
           pos = (fromIntegral x, fromIntegral y)
           -- FBM: 3 octaves with decreasing amplitude
           !n1 = sf1 pos
           !n2 = sf2 pos
           !n3 = sf3 pos
           !baseTex = n1 * 0.5 + n2 * 0.3 + n3 * 0.2  -- [0,1]
           !largescale = sfLarge pos  -- [0,1]
           -- Combine fine fiber + large scale undulation
           !combined = baseTex * 0.6 + largescale * 0.4  -- [0,1]
           -- Paper grain more visible in midtones
           !lum = (0.2126 * fromIntegral r + 0.7152 * fromIntegral g + 0.0722 * fromIntegral b) / 255.0
           !midtoneBias = 1.0 - 2.0 * abs (lum - 0.5)  -- peaks at lum=0.5
           !effectiveInt = intensity * (0.4 + 0.6 * midtoneBias)
           -- Multiplicative application (paper absorbs light)
           !factor = 1.0 + (combined - 0.5) * effectiveInt
           f v = clamp $ fromIntegral v * factor
       in PixelRGBA8 (round $ f r) (round $ f g) (round $ f b) a
     ) w h

-- | Crackle (Simplex)
crackle :: Double -> Effect
crackle amount img =
  let (Image w h _) = img
      sf = simplex 67890 0.05
  in generateImage (\x y ->
       let PixelRGBA8 r g b a = pixelAt img x y
           n = sf (fromIntegral x, fromIntegral y) -- 0..1
           -- Threshold for cracks
           isCrack = n > (1.0 - amount * 0.1)
       in if isCrack
          then PixelRGBA8 (round $ fromIntegral r * 0.7) (round $ fromIntegral g * 0.7) (round $ fromIntegral b * 0.7) a
          else PixelRGBA8 r g b a
     ) w h

-- | Marble
marble :: Double -> Double -> Effect
marble intensity scale img =
  let (Image w h _) = img
  in generateImage (\x y ->
       let n = sin (fromIntegral x * scale / 10 + fromIntegral y * scale / 20)
           dx = n * intensity * 50
           dy = n * intensity * 50
       in sampleBilinear img (fromIntegral x + dx) (fromIntegral y + dy)
     ) w h

-- | Wood
wood :: Double -> Double -> Effect
wood intensity grainWidth img =
  let (Image w h _) = img
      cx = fromIntegral w / 2
      cy = fromIntegral h / 2
  in generateImage (\x y ->
       let dx = fromIntegral x - cx
           dy = fromIntegral y - cy
           dist = sqrt (dx*dx + dy*dy)
           grain = sin (dist / grainWidth)
           PixelRGBA8 r g b a = pixelAt img x y
           f v = clamp $ fromIntegral v * (1 + grain * intensity * 0.3)
       in PixelRGBA8 (round $ f r) (round $ f g) (round $ f b) a
     ) w h

-- | Plastic
plastic :: Double -> Double -> Double -> Double -> Effect
plastic specular tintR tintG tintB img =
  let (Image w h _) = img
      blurred = gaussianBlur 2.0 img -- For normals
  in generateImage (\x y ->
       let PixelRGBA8 r g b a = pixelAt img x y
           -- Fake normal from luminance gradient
           l = if x > 0 then luminance (pixelAt blurred (x-1) y) else 0
           r_ = if x < w-1 then luminance (pixelAt blurred (x+1) y) else 0
           t = if y > 0 then luminance (pixelAt blurred x (y-1)) else 0
           d = if y < h-1 then luminance (pixelAt blurred x (y+1)) else 0
           nx = (l - r_)
           ny = (t - d)
           nz = 1.0
           -- Light dir
           lx = 0.5
           ly = 0.5
           lz = 1.0
           -- Specular
           dot = nx*lx + ny*ly + nz*lz
           spec = if dot > 0 then dot ** 20 * specular * 255 else 0
           
           tint r' = r' + tintR * 0.1
           tint g' = g' + tintG * 0.1
           tint b' = b' + tintB * 0.1
           
       in PixelRGBA8 (round $ clamp $ tint (fromIntegral r) + spec) 
                     (round $ clamp $ tint (fromIntegral g) + spec) 
                     (round $ clamp $ tint (fromIntegral b) + spec) a
     ) w h

luminance :: PixelRGBA8 -> Double
luminance (PixelRGBA8 r g b _) = (fromIntegral r + fromIntegral g + fromIntegral b) / 3.0 / 255.0

-- | Metal (Simplex directional)
metal :: Double -> String -> Effect
metal intensity direction img =
  let (Image w h _) = img
      sf = simplex 11223 0.1
  in generateImage (\x y ->
       let PixelRGBA8 r g b a = pixelAt img x y
           nVal = case direction of
             "horizontal" -> sf (fromIntegral x, fromIntegral y * 0.1)
             "vertical"   -> sf (fromIntegral x * 0.1, fromIntegral y)
             _            -> sf (fromIntegral x * 0.5, fromIntegral y * 0.5)
           n = (nVal * 2.0 - 1.0)
           f v = clamp $ fromIntegral v + n * intensity * 40
       in PixelRGBA8 (round $ f r) (round $ f g) (round $ f b) a
     ) w h

-- | Glass
glass :: Double -> Effect
glass refIndex img =
  let (Image w h _) = img
  in generateImage (\x y ->
       let dx = sin (fromIntegral y * 0.1) * refIndex * 10
           dy = cos (fromIntegral x * 0.1) * refIndex * 10
       in sampleBilinear img (fromIntegral x + dx) (fromIntegral y + dy)
     ) w h

-- | Oil Paint (Posterize + Blur)
oilPaint :: Int -> Int -> Effect
oilPaint radius levels img
  | radius <= 0 = img
  | otherwise =
      -- Blur first to create blobs, then posterize
      let blurred = gaussianBlur (fromIntegral radius) img
          (Image w h _) = blurred
      in generateImage (\x y ->
           let PixelRGBA8 r g b a = pixelAt blurred x y
               f v = fromIntegral (round (fromIntegral v / 255.0 * fromIntegral levels) * 255 `div` levels)
           in PixelRGBA8 (f r) (f g) (f b) a
         ) w h

-- | Charcoal
charcoal :: Double -> Effect
charcoal intensity img =
  let (Image w h _) = img
      gray = grayscale "luma" img
      edge = gaussianBlur 1.0 gray
  in generateImage (\x y ->
       let PixelRGBA8 v1 _ _ _ = pixelAt gray x y
           PixelRGBA8 v2 _ _ _ = pixelAt edge x y
           diff = abs (fromIntegral v1 - fromIntegral v2)
           val = 255 - diff * intensity * 5
       in PixelRGBA8 (round $ clamp val) (round $ clamp val) (round $ clamp val) 255
     ) w h

-- | Neon
neon :: Double -> Double -> Double -> Double -> Effect
neon threshold nr ng nb img =
  let (Image w h _) = img
      edges = generateImage (\x y ->
          if x > 0 && y > 0 && x < w-1 && y < h-1 then
             let l = luminance (pixelAt img x y)
                 l_left = luminance (pixelAt img (x-1) y)
                 l_top = luminance (pixelAt img x (y-1))
                 diff = abs (l - l_left) + abs (l - l_top)
             in if diff > threshold then 1.0 else 0.0 :: Float
          else 0.0
        ) w h
      
      blurredEdges = gaussianBlur 2.0 (imageFromMap w h edges)
      
      imageFromMap w' h' m = generateImage (\x y -> 
          let v = realToFrac (pixelAt m x y) :: Double
          in PixelRGBA8 (round $ v * nr) (round $ v * ng) (round $ v * nb) 255
        ) w' h'
        
  in generateImage (\x y ->
          let PixelRGBA8 r1 g1 b1 a1 = pixelAt img x y
              PixelRGBA8 r2 g2 b2 _ = pixelAt blurredEdges x y
          in PixelRGBA8 (round $ clamp $ fromIntegral r1 + fromIntegral r2)
                        (round $ clamp $ fromIntegral g1 + fromIntegral g2)
                        (round $ clamp $ fromIntegral b1 + fromIntegral b2)
                        a1
        ) w h

-- | Ink
ink :: Double -> Effect
ink intensity img =
  let (Image w h _) = img
      gray = grayscale "luma" img
  in generateImage (\x y ->
       if x > 0 && y > 0 && x < w-1 && y < h-1 then
         let l = luminance (pixelAt gray x y)
             l_left = luminance (pixelAt gray (x-1) y)
             l_top = luminance (pixelAt gray x (y-1))
             diff = abs (l - l_left) + abs (l - l_top)
         in if diff > (1.0 - intensity) * 0.1 
            then PixelRGBA8 0 0 0 255
            else PixelRGBA8 255 255 255 255
       else PixelRGBA8 255 255 255 255
     ) w h


-- | Bilinear sampling
sampleBilinear :: Image PixelRGBA8 -> Double -> Double -> PixelRGBA8
sampleBilinear img x y =
  let (Image w h _) = img
      x' = max 0 (min (fromIntegral w - 1.001) x)
      y' = max 0 (min (fromIntegral h - 1.001) y)
      ix = floor x'
      iy = floor y'
      fx = x' - fromIntegral ix
      fy = y' - fromIntegral iy
      
      p00 = pixelAt img ix iy
      p10 = pixelAt img (ix+1) iy
      p01 = pixelAt img ix (iy+1)
      p11 = pixelAt img (ix+1) (iy+1)
      
      mix :: Word8 -> Word8 -> Word8 -> Word8 -> Word8
      mix v00 v10 v01 v11 =
        let top = fromIntegral v00 * (1 - fx) + fromIntegral v10 * fx
            bot = fromIntegral v01 * (1 - fx) + fromIntegral v11 * fx
        in round (top * (1 - fy) + bot * fy)
        
      PixelRGBA8 r00 g00 b00 a00 = p00
      PixelRGBA8 r10 g10 b10 a10 = p10
      PixelRGBA8 r01 g01 b01 a01 = p01
      PixelRGBA8 r11 g11 b11 a11 = p11
      
  in PixelRGBA8 (mix r00 r10 r01 r11)
                (mix g00 g10 g01 g11)
                (mix b00 b10 b01 b11)
                (mix a00 a10 a01 a11)