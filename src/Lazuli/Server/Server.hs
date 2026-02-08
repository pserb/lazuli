{-# LANGUAGE OverloadedStrings #-}
module Lazuli.Server.Server
  ( startServer
  ) where

import Web.Scotty
import Network.WebSockets
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Handler.Warp (run)
import Network.Wai (Application)
import Data.Aeson (decode, encode)
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as BL
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, void, when)
import Control.Exception (SomeException)
import qualified Control.Exception as E
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Random (randomRIO)

import Lazuli.Server.Types
import Lazuli.Server.Routes (apiRoutes)
import Lazuli.Server.RenderQueue (RenderQueue, newRenderQueue, submitRender, PreviewSettings(..))
import Lazuli.Style (styleByName, allStyles)
import Lazuli.Palette (paletteByName)
import Lazuli.Effect (parseEffect)
import Lazuli.Render (renderToPngBytesAA)

-- | Start the web server on the given port
startServer :: Int -> IO ()
startServer port = do
  putStrLn $ "Starting Lazuli server on http://localhost:" ++ show port
  putStrLn $ "WebSocket endpoint: ws://localhost:" ++ show port ++ "/ws"

  -- Create render queue
  queue <- newRenderQueue

  -- Build HTTP application
  httpApp' <- httpApp queue

  -- Build WAI application with WebSocket support
  let wsApp = websocketsOr defaultConnectionOptions (wsServer queue) httpApp'

  -- Start server
  run port wsApp

-- | HTTP application (Scotty)
httpApp :: RenderQueue -> IO Application
httpApp _ = scottyApp $ do
  -- API routes
  apiRoutes
  
  -- Serve static files
  get "/" $ file "static/index.html"
  get "/css/:filename" $ do
    filename <- pathParam "filename"
    file $ "static/css/" ++ T.unpack filename
  get "/js/:filename" $ do
    filename <- pathParam "filename"
    file $ "static/js/" ++ T.unpack filename

-- | WebSocket server
wsServer :: RenderQueue -> ServerApp
wsServer queue pending = do
  conn <- acceptRequest pending
  putStrLn "WebSocket connection accepted"
  
  -- Handle messages in a loop
  forever $ do
    msg <- receiveData conn
    putStrLn $ "Received raw message: " ++ T.unpack msg
    case A.decode (TLE.encodeUtf8 (TL.fromStrict msg)) of
      Just req -> do
        putStrLn $ "Received render request: " ++ style req
        handleRenderRequest conn req
      Nothing -> do
        putStrLn $ "Failed to decode request. Raw message: " ++ T.unpack msg
        sendTextData conn $ encodeResponse $ PreviewResponse Error Nothing "" Nothing (Just "Invalid request format")
    `E.catch` \(_ :: SomeException) -> do
      putStrLn "WebSocket error, closing connection"
      return ()

-- | Handle a render request
handleRenderRequest :: Connection -> PreviewRequest -> IO ()
handleRenderRequest conn req = do
  startTime <- getCurrentTime
  
  -- Send "rendering" status
  sendTextData conn $ encodeResponse $ PreviewResponse Rendering Nothing "" Nothing Nothing
  
  -- Perform the actual render
  result <- renderPreview req
  
  endTime <- getCurrentTime
  let renderTime = round $ realToFrac (diffUTCTime endTime startTime) * 1000
  
  case result of
    Right (imageData, cmd) -> do
      -- Convert to base64 (lazy ByteString to strict Text via UTF-8)
      let base64Image = TE.decodeUtf8 $ BL.toStrict $ B64.encode imageData
      sendTextData conn $ encodeResponse $ PreviewResponse Complete (Just base64Image) cmd (Just renderTime) Nothing
    Left err -> do
      sendTextData conn $ encodeResponse $ PreviewResponse Error Nothing "" (Just renderTime) (Just err)

-- | Render a preview image
renderPreview :: PreviewRequest -> IO (Either String (BL.ByteString, String))
renderPreview req = do
  -- Look up style and palette
  case (styleByName (style req), paletteByName (palette req)) of
    (Just styleFn, Just paletteRaw) -> do
      -- Apply invert if needed
      let palette = if invert req 
                    then (\t -> paletteRaw (1 - t)) 
                    else paletteRaw
      
      -- Parse effects
      case mapM parseEffect (effects req) of
        Right effects -> do
          -- Create the color field
          let field = styleFn (seed req) palette (freq req)
          
          -- Render at 480x270 with 1 sample (fast preview)
          imageData <- renderToPngBytesAA effects 1 480 270 field
          
          -- Build command string
          let cmd = buildCliCommand req
          
          return $ Right (imageData, cmd)
        Left err -> return $ Left $ "Error parsing effects: " ++ err
    (Nothing, _) -> return $ Left $ "Unknown style: " ++ style req
    (_, Nothing) -> return $ Left $ "Unknown palette: " ++ palette req

-- | Build CLI command from request
buildCliCommand :: PreviewRequest -> String
buildCliCommand req = 
  "lazuli " ++
  "--style " ++ style req ++ " " ++
  "--palette " ++ palette req ++ " " ++
  "--seed " ++ show (seed req) ++ " " ++
  "--freq " ++ show (freq req) ++ " " ++
  (if invert req then "--invert " else "") ++
  concatMap (\e -> "--effect " ++ show e ++ " ") (effects req) ++
  "--set-wallpaper"

-- | Helper to encode response as JSON
encodeResponse :: PreviewResponse -> T.Text
encodeResponse resp = TL.toStrict $ TLE.decodeUtf8 $ encode resp
