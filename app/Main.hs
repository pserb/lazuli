module Main where

import Lazuli.Render (renderToFileAA)
import Lazuli.Gallery (generateGallery)
import Lazuli.Palette (paletteByName, allPalettes, paletteNames)
import Lazuli.Style (styleByName, allStyles)
import Lazuli.Effect (Effect, parseEffect, effectDescriptions)
import Options.Applicative
import System.Random (randomRIO)
import System.Exit (exitSuccess, exitFailure, ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.Directory (makeAbsolute)
import Data.Maybe (fromMaybe)
import Control.Monad (when, unless)
import Text.Read (readMaybe)
import GHC.Conc (getNumProcessors)

------------------------------------------------------------------------
-- CLI Options
------------------------------------------------------------------------

data Options = Options
  { optStyle        :: Maybe String
  , optPalette      :: Maybe String
  , optSeed         :: Maybe Int
  , optWidth        :: Maybe Int
  , optHeight       :: Maybe Int
  , optRes          :: Maybe (Int, Int)
  , optOutput       :: Maybe FilePath
  , optPreview      :: Bool
  , optRandom       :: Bool
  , optGallery      :: Maybe Int
  , optListStyles   :: Bool
  , optListPalettes :: Bool
  , optListEffects  :: Bool
  , optSetWallpaper :: Bool
  , optJobs         :: Maybe Int
  , optSamples      :: Int
  , optEffects      :: [String]
  }

optParser :: Parser Options
optParser = Options
  <$> optional (strOption (long "style" <> short 's' <> metavar "STYLE" <> help "Style name"))
  <*> optional (strOption (long "palette" <> short 'p' <> metavar "PALETTE" <> help "Palette name"))
  <*> optional (option auto (long "seed" <> short 'S' <> metavar "INT" <> help "Random seed"))
  <*> optional (option auto (long "width" <> metavar "PX" <> help "Output width (default: screen width)"))
  <*> optional (option auto (long "height" <> metavar "PX" <> help "Output height (default: screen height)"))
  <*> optional (option parseRes (long "res" <> metavar "WxH" <> help "Resolution (e.g., 1920x1080)"))
  <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Output file"))
  <*> switch (long "preview" <> help "Render at 1/4 resolution")
  <*> switch (long "random" <> help "Randomize style, palette, and seed")
  <*> optional (option auto (long "gallery" <> metavar "N" <> help "Generate N wallpapers as HTML gallery"))
  <*> switch (long "list-styles" <> help "List available styles")
  <*> switch (long "list-palettes" <> help "List available palettes")
  <*> switch (long "list-effects" <> help "List available effects")
  <*> switch (long "set-wallpaper" <> help "Set as desktop wallpaper (macOS)")
  <*> optional (option auto (long "jobs" <> short 'j' <> metavar "N" <> help "Parallel threads"))
  <*> option auto (long "samples" <> value 4 <> metavar "N" <> help "Anti-aliasing samples (1=off, 4=RGSS)")
  <*> many (strOption (long "effect" <> metavar "EFFECT" <> help "Apply effect (e.g. blur:3)"))

parseRes :: ReadM (Int, Int)
parseRes = eitherReader $ \s -> case break (== 'x') s of
  (w, 'x':h) -> case (readMaybe w, readMaybe h) of
    (Just wn, Just hn) -> Right (wn, hn)
    _                  -> Left "Expected WxH format (e.g., 1920x1080)"
  _ -> Left "Expected WxH format (e.g., 1920x1080)"

------------------------------------------------------------------------
-- Screen resolution detection (macOS)
------------------------------------------------------------------------

-- | Detect the main screen's native resolution on macOS via NSScreen.
-- Returns Nothing if detection fails (non-macOS, headless, etc.).
detectScreenResolution :: IO (Maybe (Int, Int))
detectScreenResolution = do
  let script = concat
        [ "ObjC.import('AppKit');"
        , "var s = $.NSScreen.mainScreen;"
        , "var f = s.frame;"
        , "var sf = s.backingScaleFactor;"
        , "'' + Math.round(f.size.width * sf) + 'x' + Math.round(f.size.height * sf)"
        ]
  (exitCode, out, _err) <- readProcessWithExitCode "osascript" ["-l", "JavaScript", "-e", script] ""
  pure $ case exitCode of
    ExitSuccess   -> parseWxH (strip out)
    ExitFailure _ -> Nothing
  where
    strip = reverse . dropWhile (== '\n') . reverse
    parseWxH s = case break (== 'x') s of
      (w, 'x':h) -> case (readMaybe w, readMaybe h) of
        (Just wn, Just hn) | wn > 0 && hn > 0 -> Just (wn, hn)
        _ -> Nothing
      _ -> Nothing

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- execParser (info (optParser <**> helper)
    (fullDesc <> progDesc "Lazuli - generative wallpaper engine"))

  -- Parse effects first to catch errors early
  effects <- case mapM parseEffect (optEffects opts) of
    Right effs -> pure effs
    Left err   -> do
      putStrLn $ "Error parsing effects: " ++ err
      exitFailure

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

  -- Handle --list-effects
  when (optListEffects opts) $ do
    putStrLn "Available effects:"
    mapM_ (\(name, desc) -> putStrLn $ "  " ++ name ++ " - " ++ desc) effectDescriptions
    exitSuccess

  -- Handle --gallery
  case optGallery opts of
    Just n -> do
      numCores <- getNumProcessors
      let jobs = fromMaybe numCores (optJobs opts)
          outPath = fromMaybe "gallery/gallery.html" (optOutput opts)
      putStrLn $ "Rendering " ++ show n ++ " thumbnails with " ++ show jobs ++ " threads..."
      generateGallery n allStyles allPalettes effects 480 270 jobs outPath
      absPath <- makeAbsolute outPath
      putStrLn $ "Gallery written to " ++ absPath
      _ <- readProcessWithExitCode "open" [absPath] ""
      pure ()
      exitSuccess
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

  -- Detect screen resolution for default sizing
  screenRes <- detectScreenResolution
  let (defaultW, defaultH) = fromMaybe (1920, 1080) screenRes

  -- Resolve resolution: --res overrides --width/--height, fallback to screen
  let (baseW, baseH) = case optRes opts of
        Just (rw, rh) -> (rw, rh)
        Nothing       -> (fromMaybe defaultW (optWidth opts), fromMaybe defaultH (optHeight opts))
      (w, h) = if optPreview opts
               then (baseW `div` 4, baseH `div` 4)
               else (baseW, baseH)

  let outFile = fromMaybe ("lazuli-" ++ show seed ++ ".png") (optOutput opts)

  -- Anti-aliasing: force 1 sample in preview mode for speed
  let samples = if optPreview opts then 1 else optSamples opts

  putStrLn $ "Generating: style=" ++ styleName ++ " palette=" ++ paletteName ++ " seed=" ++ show seed
  putStrLn $ "Resolution: " ++ show w ++ "x" ++ show h ++ " (samples=" ++ show samples ++ ")"
  unless (null (optEffects opts)) $
    putStrLn $ "Effects: " ++ unwords (optEffects opts)

  let field = style seed palette
  renderToFileAA effects outFile samples w h field

  putStrLn $ "Written to " ++ outFile

  -- Handle --set-wallpaper
  when (optSetWallpaper opts) $ do
    absPath <- makeAbsolute outFile
    let script = "tell application \"System Events\" to tell every desktop to set picture to POSIX file " ++ show absPath
    (exitCode, _out, err) <- readProcessWithExitCode "osascript" ["-e", script] ""
    case exitCode of
      ExitSuccess   -> putStrLn $ "Wallpaper set to " ++ absPath
      ExitFailure _ -> putStrLn $ "Failed to set wallpaper: " ++ err