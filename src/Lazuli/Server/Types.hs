{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Lazuli.Server.Types
  ( PreviewRequest(..)
  , PreviewResponse(..)
  , RenderStatus(..)
  , StyleInfo(..)
  , PaletteInfo(..)
  , EffectInfo(..)
  , EffectParam(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON, object, (.=))
import Data.Text (Text)

-- | Client request for a preview render
data PreviewRequest = PreviewRequest
  { style     :: String
  , palette   :: String
  , seed      :: Int
  , freq      :: Double
  , invert    :: Bool
  , effects   :: [String]
  } deriving (Show, Generic)

instance ToJSON PreviewRequest
instance FromJSON PreviewRequest

-- | Server response with rendered image
data PreviewResponse = PreviewResponse
  { respStatus     :: RenderStatus
  , respImage      :: Maybe Text  -- ^ Base64 encoded PNG
  , respCommand    :: String      -- ^ CLI command to reproduce
  , respRenderTime :: Maybe Int   -- ^ Render time in milliseconds
  , respError      :: Maybe String
  } deriving (Show, Generic)

instance ToJSON PreviewResponse where
  toJSON resp = object
    [ "status"     .= respStatus resp
    , "image"      .= respImage resp
    , "command"    .= respCommand resp
    , "renderTime" .= respRenderTime resp
    , "error"      .= respError resp
    ]

-- | Status of render operation
data RenderStatus
  = Rendering
  | Complete
  | Error
  deriving (Show, Generic)

instance ToJSON RenderStatus

-- | Style metadata for API
data StyleInfo = StyleInfo
  { styleName        :: String
  , styleDescription :: String
  } deriving (Show, Generic)

instance ToJSON StyleInfo

-- | Palette metadata for API
data PaletteInfo = PaletteInfo
  { paletteName   :: String
  , paletteColors :: [String]
  } deriving (Show, Generic)

instance ToJSON PaletteInfo

-- | Effect metadata for API
data EffectInfo = EffectInfo
  { effectName        :: String
  , effectDescription :: String
  , effectParams      :: [EffectParam]
  } deriving (Show, Generic)

instance ToJSON EffectInfo

-- | Effect parameter specification
data EffectParam = EffectParam
  { paramName    :: String
  , paramType    :: String  -- ^ "float", "int", "bool"
  , paramMin     :: Maybe Double
  , paramMax     :: Maybe Double
  , paramDefault :: Maybe String
  } deriving (Show, Generic)

instance ToJSON EffectParam
