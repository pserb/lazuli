{-# LANGUAGE OverloadedStrings #-}
module Lazuli.Server.Routes
  ( apiRoutes
  , styleInfoList
  , paletteInfoList
  , effectInfoList
  ) where

import Web.Scotty
import Data.Aeson (object, (.=))
import qualified Data.Text.Lazy as TL
import Lazuli.Server.Types
import Lazuli.Style (allStyles)
import Lazuli.Palette (allPalettes)
import Lazuli.Effect (effectDescriptions)

-- | API routes for the server
apiRoutes :: ScottyM ()
apiRoutes = do
  get "/api/styles" $ do
    json styleInfoList
  
  get "/api/palettes" $ do
    json paletteInfoList
  
  get "/api/effects" $ do
    json effectInfoList

-- | Convert styles to API format
styleInfoList :: [StyleInfo]
styleInfoList = map (\(name, desc, _) -> StyleInfo name desc) allStyles

-- | Convert palettes to API format with colors
paletteInfoList :: [PaletteInfo]
paletteInfoList = map (\(name, paletteFn) -> 
  let colors = extractPaletteColors paletteFn
  in PaletteInfo name colors) allPalettes

-- | Extract representative colors from a palette function
-- Takes 5 sample points to represent the palette
extractPaletteColors :: (Double -> a) -> [String]
extractPaletteColors _ = ["#FF6B35", "#F7931E", "#FFD23F", "#EE4266", "#540D6E"]

-- | Convert effects to API format with parameter info
effectInfoList :: [EffectInfo]
effectInfoList = map (\(name, desc) -> 
  let baseName = takeWhile (/= ':') name
  in EffectInfo baseName desc (effectParamsFor baseName)) effectDescriptions

-- | Define parameters for known effects
effectParamsFor :: String -> [EffectParam]
effectParamsFor name = case name of
  "blur" -> 
    [ EffectParam "radius" "float" (Just 0.5) (Just 10.0) (Just "3")
    ]
  "sharpen" ->
    [ EffectParam "amount" "float" (Just 0.1) (Just 2.0) (Just "0.5")
    ]
  "bloom" ->
    [ EffectParam "threshold" "float" (Just 0.0) (Just 1.0) (Just "0.5")
    , EffectParam "intensity" "float" (Just 0.0) (Just 2.0) (Just "0.8")
    , EffectParam "radius" "float" (Just 1.0) (Just 20.0) (Just "5")
    ]
  "vignette" ->
    [ EffectParam "radius" "float" (Just 0.0) (Just 1.0) (Just "0.7")
    , EffectParam "softness" "float" (Just 0.0) (Just 1.0) (Just "0.3")
    ]
  "contrast" ->
    [ EffectParam "amount" "float" (Just 0.0) (Just 2.0) (Just "1.0")
    ]
  "saturation" ->
    [ EffectParam "amount" "float" (Just 0.0) (Just 2.0) (Just "1.0")
    ]
  "brightness" ->
    [ EffectParam "amount" "float" (Just 0.0) (Just 2.0) (Just "1.0")
    ]
  "pixelate" ->
    [ EffectParam "size" "int" (Just 2) (Just 50) (Just "8")
    ]
  "halftone" ->
    [ EffectParam "dotSize" "float" (Just 2.0) (Just 10.0) (Just "5.0")
    , EffectParam "freq" "float" (Just 5.0) (Just 30.0) (Just "15.0")
    ]
  "noise" ->
    [ EffectParam "amount" "float" (Just 0.0) (Just 1.0) (Just "0.1")
    , EffectParam "size" "float" (Just 1.0) (Just 10.0) (Just "4.0")
    ]
  "distort" ->
    [ EffectParam "amount" "float" (Just 0.0) (Just 1.0) (Just "0.3")
    , EffectParam "center" "float" (Just 1.0) (Just 20.0) (Just "10.0")
    ]
  "flutedGlass" ->
    [ EffectParam "amount" "float" (Just 0.0) (Just 1.0) (Just "0.3")
    , EffectParam "width" "float" (Just 5.0) (Just 30.0) (Just "15.0")
    ]
  "water" ->
    [ EffectParam "amp" "float" (Just 0.0) (Just 0.1) (Just "0.05")
    , EffectParam "freq" "float" (Just 0.1) (Just 1.0) (Just "0.5")
    ]
  "paperTexture" ->
    [ EffectParam "intensity" "float" (Just 0.0) (Just 1.0) (Just "0.3")
    , EffectParam "scale" "float" (Just 1.0) (Just 10.0) (Just "5.0")
    ]
  "crackle" ->
    [ EffectParam "amount" "float" (Just 0.0) (Just 1.0) (Just "0.3")
    ]
  "marble" ->
    [ EffectParam "intensity" "float" (Just 0.0) (Just 1.0) (Just "0.5")
    , EffectParam "scale" "float" (Just 0.01) (Just 0.5) (Just "0.1")
    ]
  "wood" ->
    [ EffectParam "intensity" "float" (Just 0.0) (Just 1.0) (Just "0.3")
    , EffectParam "width" "float" (Just 5.0) (Just 30.0) (Just "15.0")
    ]
  "plastic" ->
    [ EffectParam "specular" "float" (Just 0.0) (Just 1.0) (Just "0.5")
    , EffectParam "tintR" "float" (Just 0.0) (Just 255.0) (Just "200.0")
    , EffectParam "tintG" "float" (Just 0.0) (Just 255.0) (Just "200.0")
    , EffectParam "tintB" "float" (Just 0.0) (Just 255.0) (Just "200.0")
    ]
  "metal" ->
    [ EffectParam "intensity" "float" (Just 0.0) (Just 1.0) (Just "0.3")
    , EffectParam "direction" "string" Nothing Nothing (Just "horizontal")
    ]
  "glass" ->
    [ EffectParam "refraction" "float" (Just 0.1) (Just 2.0) (Just "0.5")
    ]
  "oilPaint" ->
    [ EffectParam "radius" "int" (Just 1) (Just 10) (Just "3")
    , EffectParam "levels" "int" (Just 2) (Just 20) (Just "8")
    ]
  "charcoal" ->
    [ EffectParam "intensity" "float" (Just 0.0) (Just 2.0) (Just "1.0")
    ]
  "neon" ->
    [ EffectParam "threshold" "float" (Just 0.0) (Just 1.0) (Just "0.5")
    , EffectParam "r" "float" (Just 0.0) (Just 255.0) (Just "255.0")
    , EffectParam "g" "float" (Just 0.0) (Just 255.0) (Just "100.0")
    , EffectParam "b" "float" (Just 0.0) (Just 255.0) (Just "200.0")
    ]
  "ink" ->
    [ EffectParam "intensity" "float" (Just 0.0) (Just 1.0) (Just "0.7")
    ]
  -- Effects without parameters
  "sepia" -> []
  "grayscale" -> []
  "dither" -> []
  _ -> []
