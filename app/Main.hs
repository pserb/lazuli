module Main where

import Lazuli.Render (renderToFileAA)
import Lazuli.Gallery (generateGallery)
import Lazuli.Palette (paletteByName, allPalettes, paletteNames)
import Lazuli.Style (styleByName, allStyles)
import Options.Applicative
import System.Random (randomRIO)
import System.Exit (exitSuccess)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Text.Read (readMaybe)

------------------------------------------------------------------------
-- CLI Options
------------------------------------------------------------------------

data Options = Options
  { optStyle        :: Maybe String
  , optPalette      :: Maybe String
  , optSeed         :: Maybe Int
  , optWidth        :: Int
  , optHeight       :: Int
  , optRes          :: Maybe (Int, Int)
  , optOutput       :: Maybe FilePath
  , optPreview      :: Bool
  , optRandom       :: Bool
  , optGallery      :: Maybe Int
  , optListStyles   :: Bool
  , optListPalettes :: Bool
  , optSetWallpaper :: Bool
  , optJobs         :: Maybe Int
  , optSamples      :: Int
  }

optParser :: Parser Options
optParser = Options
  <$> optional (strOption (long "style" <> short 's' <> metavar "STYLE" <> help "Style name"))
  <*> optional (strOption (long "palette" <> short 'p' <> metavar "PALETTE" <> help "Palette name"))
  <*> optional (option auto (long "seed" <> short 'S' <> metavar "INT" <> help "Random seed"))
  <*> option auto (long "width" <> value 1920 <> help "Output width")
  <*> option auto (long "height" <> value 1080 <> help "Output height")
  <*> optional (option parseRes (long "res" <> metavar "WxH" <> help "Resolution (e.g., 1920x1080)"))
  <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Output file"))
  <*> switch (long "preview" <> help "Render at 1/4 resolution")
  <*> switch (long "random" <> help "Randomize style, palette, and seed")
  <*> optional (option auto (long "gallery" <> metavar "N" <> help "Generate N wallpapers as HTML gallery"))
  <*> switch (long "list-styles" <> help "List available styles")
  <*> switch (long "list-palettes" <> help "List available palettes")
  <*> switch (long "set-wallpaper" <> help "Set as desktop wallpaper (macOS)")
  <*> optional (option auto (long "jobs" <> short 'j' <> metavar "N" <> help "Parallel threads"))
  <*> option auto (long "samples" <> value 4 <> metavar "N" <> help "Anti-aliasing samples (1=off, 4=RGSS)")

parseRes :: ReadM (Int, Int)
parseRes = eitherReader $ \s -> case break (== 'x') s of
  (w, 'x':h) -> case (readMaybe w, readMaybe h) of
    (Just wn, Just hn) -> Right (wn, hn)
    _                  -> Left "Expected WxH format (e.g., 1920x1080)"
  _ -> Left "Expected WxH format (e.g., 1920x1080)"

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- execParser (info (optParser <**> helper)
    (fullDesc <> progDesc "Lazuli - generative wallpaper engine"))

  -- Handle --list-styles
  when (optListStyles opts) $ do
    putStrLn "Available styles:"
    mapM_ (\(name, desc, _) -> putStrLn $ "  " ++ name ++ " - " ++ desc) allStyles
    exitSuccess

  -- Handle --list-palettes
  when (optListPalettes opts) $ do
    putStrLn "Available palettes:"
    mapM_ putStrLn paletteNames
    exitSuccess

  -- Handle --gallery
  case optGallery opts of
    Just n -> do
      let outPath = fromMaybe "gallery.html" (optOutput opts)
      generateGallery n allStyles allPalettes 480 270 outPath
      putStrLn $ "Gallery written to " ++ outPath
      exitSuccess
    Nothing -> pure ()

  -- Handle --jobs (informational only for now)
  case optJobs opts of
    Just j  -> putStrLn $ "Using " ++ show j ++ " threads (parallelism not yet implemented)"
    Nothing -> pure ()

  -- Resolve seed, style, palette (--random overrides explicit values)
  let forceRandom = optRandom opts

  seed <- case optSeed opts of
    Just s | not forceRandom -> pure s
    _                        -> randomRIO (0, 999999 :: Int)

  let sNames = map (\(n,_,_) -> n) allStyles
      pNames = map fst allPalettes

  styleName <- case optStyle opts of
    Just s | not forceRandom -> pure s
    _ -> do
      idx <- randomRIO (0, length sNames - 1)
      pure (sNames !! idx)

  paletteName <- case optPalette opts of
    Just p | not forceRandom -> pure p
    _ -> do
      idx <- randomRIO (0, length pNames - 1)
      pure (pNames !! idx)

  let style   = fromMaybe (error $ "Unknown style: " ++ styleName)   (styleByName styleName)
      palette = fromMaybe (error $ "Unknown palette: " ++ paletteName) (paletteByName paletteName)

  -- Resolve resolution: --res overrides --width/--height, --preview quarters it
  let (baseW, baseH) = case optRes opts of
        Just (rw, rh) -> (rw, rh)
        Nothing       -> (optWidth opts, optHeight opts)
      (w, h) = if optPreview opts
               then (baseW `div` 4, baseH `div` 4)
               else (baseW, baseH)

  let outFile = fromMaybe ("lazuli-" ++ show seed ++ ".png") (optOutput opts)

  -- Anti-aliasing: force 1 sample in preview mode for speed
  let samples = if optPreview opts then 1 else optSamples opts

  putStrLn $ "Generating: style=" ++ styleName ++ " palette=" ++ paletteName ++ " seed=" ++ show seed
  putStrLn $ "Resolution: " ++ show w ++ "x" ++ show h ++ " (samples=" ++ show samples ++ ")"

  let field = style seed palette
  renderToFileAA outFile samples w h field

  putStrLn $ "Written to " ++ outFile

  -- Handle --set-wallpaper (stretch goal)
  when (optSetWallpaper opts) $
    putStrLn "Set wallpaper: not yet supported (requires process dependency)"
