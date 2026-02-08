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
    [ EffectParam "strength" "float" (Just 0.0) (Just 1.0) (Just "0.35")
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
    [ EffectParam "size" "int" (Just 2) (Just 20) (Just "6")
    ]
  "noise" ->
    [ EffectParam "amount" "float" (Just 0.0) (Just 1.0) (Just "0.1")
    ]
  _ -> []  -- Effects without parameters
