{-# LANGUAGE OverloadedStrings #-}
module Lazuli.Gallery
  ( generateGallery
  ) where

import Lazuli.Types (Palette)
import Lazuli.Style (Style)
import Lazuli.Render (renderToFile)
import Lazuli.Effect (Effect, randomEffects)
import System.Random (StdGen, mkStdGen, randomR)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Lazy as BL
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.QSem (newQSem, waitQSem, signalQSem)
import Control.Monad (unless)
import Control.Exception (bracket_, SomeException, try)
import System.Directory (createDirectoryIfMissing)

-- | Generate an HTML gallery page with thumbnail wallpapers.
-- Writes thumbnail PNGs alongside the HTML file.
generateGallery :: Int                       -- ^ number of wallpapers
                -> [(String, String, Style)] -- ^ allStyles: (name, desc, styleFn)
                -> [(String, Palette)]       -- ^ allPalettes: (name, palette)
                -> [Effect]                  -- ^ post-processing effects (if empty, random effects are used)
                -> Int -> Int                -- ^ thumbnail width, height
                -> Int                       -- ^ number of parallel threads
                -> FilePath                  -- ^ output HTML path
                -> IO ()
generateGallery n styles palettes fixedEffects tw th jobs outputPath = do
  let dir = dirOf outputPath
      entries = take n $ randomEntries (mkStdGen 12345) styles palettes fixedEffects
      indexed = zip [(0 :: Int)..] entries

  createDirectoryIfMissing True dir

  -- Render thumbnails in parallel, bounded by job count
  sem <- newQSem jobs
  dones <- mapM (\(i, (_, _, seed, styleFn, pal, effs, _)) -> do
    done <- newEmptyMVar
    _ <- forkIO $ do
      result <- try $ bracket_ (waitQSem sem) (signalQSem sem) $ do
        let field = styleFn seed pal 1.0
            thumbPath = dir ++ "/" ++ thumbName i
        renderToFile effs thumbPath tw th field
        putStrLn $ "  [" ++ show (i + 1) ++ "/" ++ show n ++ "] " ++ thumbPath
      case result of
        Left e  -> putStrLn $ "  Error rendering thumb-" ++ show i ++ ": " ++ show (e :: SomeException)
        Right _ -> pure ()
      putMVar done ()
    return done
    ) indexed
  mapM_ takeMVar dones

  -- Write HTML
  let htmlEntries =
        [ (sn, pn, sd, effNames, thumbName i)
        | (i, (sn, pn, sd, _, _, _, effNames)) <- zip [(0 :: Int)..] entries
        ]
  BL.writeFile outputPath (renderHtml $ galleryHtml htmlEntries)

-- | Generate an infinite list of random (style, palette, seed, effects) combinations.
randomEntries :: StdGen
              -> [(String, String, Style)]
              -> [(String, Palette)]
              -> [Effect]
              -> [(String, String, Int, Style, Palette, [Effect], [String])]
randomEntries gen styles palettes fixedEffects =
  let (si, g1) = randomR (0, length styles - 1) gen
      (pi', g2) = randomR (0, length palettes - 1) g1
      (seed, g3) = randomR (0, 999999 :: Int) g2
      (effs, effNames, g4) = if null fixedEffects
                             then randomEffects g3
                             else (fixedEffects, [], g3)
      (sName, _, sFn) = styles !! si
      (pName, pFn) = palettes !! pi'
  in (sName, pName, seed, sFn, pFn, effs, effNames) : randomEntries g4 styles palettes fixedEffects

-- | Thumbnail filename for index i.
thumbName :: Int -> String
thumbName i = "thumb-" ++ show i ++ ".png"

-- | Extract directory from a filepath (simple Unix-only implementation).
dirOf :: FilePath -> FilePath
dirOf path
  | '/' `elem` path = reverse . drop 1 . dropWhile (/= '/') $ reverse path
  | otherwise        = "."

------------------------------------------------------------------------
-- HTML generation via blaze-html
------------------------------------------------------------------------

galleryHtml :: [(String, String, Int, [String], String)] -> H.Html
galleryHtml entries = H.docTypeHtml $ do
  H.head $ do
    H.title "Lazuli Gallery"
    H.style $ H.toHtml galleryCss
  H.body $ do
    H.h1 "Lazuli Gallery"
    H.div ! A.class_ "grid" $
      mapM_ cardHtml entries

cardHtml :: (String, String, Int, [String], String) -> H.Html
cardHtml (styleName, paletteName, seed, effNames, thumbFile) =
  H.div ! A.class_ "card" $ do
    H.img ! A.src (H.toValue thumbFile)
    H.div ! A.class_ "info" $ do
      H.div ! A.class_ "meta" $ H.toHtml $
        "Style: " ++ styleName ++ " | Palette: " ++ paletteName ++ " | Seed: " ++ show seed
      unless (null effNames) $
        H.div ! A.class_ "effects" $ do
          H.span "Effects: "
          H.toHtml $ unwords effNames
      let effArgs = concatMap (\e -> " --effect " ++ e) effNames
      H.code $ H.toHtml $
        "lazuli --style " ++ styleName ++ " --palette " ++ paletteName ++ " --seed " ++ show seed ++ effArgs

galleryCss :: String
galleryCss = unlines
  [ "body { background: #111; color: #eee; font-family: monospace; }"
  , ".grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(480px, 1fr)); gap: 16px; padding: 16px; }"
  , ".card { background: #222; border-radius: 8px; overflow: hidden; border: 1px solid #333; }"
  , ".card img { width: 100%; display: block; }"
  , ".card .info { padding: 12px; font-size: 12px; display: flex; flex-direction: column; gap: 8px; }"
  , ".card .meta { color: #aaa; }"
  , ".card .effects { color: #3498db; font-weight: bold; }"
  , ".card .effects span { color: #eee; font-weight: normal; }"
  , ".card code { color: #0f0; background: #000; padding: 4px; border-radius: 4px; word-break: break-all; }"
  ]