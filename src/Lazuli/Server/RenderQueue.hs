{-# LANGUAGE OverloadedStrings #-}
module Lazuli.Server.RenderQueue
  ( RenderQueue
  , newRenderQueue
  , submitRender
  , PreviewSettings(..)
  ) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Lazuli.Server.Types (PreviewRequest(..))
import Data.Maybe (isNothing)

-- | Settings for preview rendering
data PreviewSettings = PreviewSettings
  { previewWidth   :: Int
  , previewHeight  :: Int
  , previewSamples :: Int
  , debounceMs     :: Int  -- ^ Debounce time in milliseconds
  }

-- Default preview settings
defaultPreviewSettings :: PreviewSettings
defaultPreviewSettings = PreviewSettings
  { previewWidth   = 480
  , previewHeight  = 270
  , previewSamples = 1
  , debounceMs     = 100
  }

-- | Internal queue state
data QueueState = QueueState
  { pendingReq  :: Maybe PreviewRequest
  , isRendering :: Bool
  }

-- | The render queue handle
newtype RenderQueue = RenderQueue (TVar QueueState)

-- | Create a new render queue
newRenderQueue :: IO RenderQueue
newRenderQueue = RenderQueue <$> newTVarIO (QueueState Nothing False)

-- | Submit a render request - will be debounced
submitRender :: RenderQueue -> PreviewRequest -> IO ()
submitRender (RenderQueue tvar) req = do
  -- Update pending request in STM
  atomically $ modifyTVar tvar $ \s -> s { pendingReq = Just req }
  -- Start debounce timer (but don't block)
  void $ forkIO $ runDebounce tvar

-- | Debounce timer - waits then processes if no new requests
runDebounce :: TVar QueueState -> IO ()
runDebounce tvar = do
  threadDelay (100 * 1000)  -- 100ms debounce
  
  -- Check if there's a pending request
  mReq <- atomically $ do
    state <- readTVar tvar
    case pendingReq state of
      Just req -> do
        -- Clear pending and mark as rendering
        writeTVar tvar $ state { pendingReq = Nothing, isRendering = True }
        return $ Just req
      Nothing -> return Nothing
  
  -- Process the request if there is one
  case mReq of
    Just req -> do
      -- In real implementation, this would call the render function
      -- For now just simulate
      putStrLn $ "Rendering: " ++ style req
      threadDelay (200 * 1000)  -- Simulate 200ms render
      atomically $ modifyTVar tvar $ \s -> s { isRendering = False }
    Nothing -> return ()
