{-# LANGUAGE OverloadedStrings #-}
module Lazuli.Gallery
  ( generateGallery
  ) where

import Lazuli.Types (Palette)
import Lazuli.Style (Style)
import Lazuli.Render (renderToFile)
import System.Random (StdGen, mkStdGen, randomR)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Lazy as BL

-- | Generate an HTML gallery page with thumbnail wallpapers.
-- Writes thumbnail PNGs alongside the HTML file.
generateGallery :: Int                       -- ^ number of wallpapers
                -> [(String, String, Style)] -- ^ allStyles: (name, desc, styleFn)
                -> [(String, Palette)]       -- ^ allPalettes: (name, palette)
                -> Int -> Int                -- ^ thumbnail width, height
                -> FilePath                  -- ^ output HTML path
                -> IO ()
generateGallery n styles palettes tw th outputPath = do
  let dir = dirOf outputPath
      entries = take n $ randomEntries (mkStdGen 12345) styles palettes

  -- Render thumbnails
  sequence_
    [ let field = styleFn seed pal
          thumbPath = dir ++ "/" ++ thumbName i
      in renderToFile thumbPath tw th field
    | (i, (_, _, seed, styleFn, pal)) <- zip [(0 :: Int)..] entries
    ]

  -- Write HTML
  let htmlEntries =
        [ (sn, pn, sd, thumbName i)
        | (i, (sn, pn, sd, _, _)) <- zip [(0 :: Int)..] entries
        ]
  BL.writeFile outputPath (renderHtml $ galleryHtml htmlEntries)

-- | Generate an infinite list of random (style, palette, seed) combinations.
randomEntries :: StdGen
              -> [(String, String, Style)]
              -> [(String, Palette)]
              -> [(String, String, Int, Style, Palette)]
randomEntries gen styles palettes =
  let (si, g1) = randomR (0, length styles - 1) gen
      (pi', g2) = randomR (0, length palettes - 1) g1
      (seed, g3) = randomR (0, 999999 :: Int) g2
      (sName, _, sFn) = styles !! si
      (pName, pFn) = palettes !! pi'
  in (sName, pName, seed, sFn, pFn) : randomEntries g3 styles palettes

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

galleryHtml :: [(String, String, Int, String)] -> H.Html
galleryHtml entries = H.docTypeHtml $ do
  H.head $ do
    H.title "Lazuli Gallery"
    H.style $ H.toHtml galleryCss
  H.body $ do
    H.h1 "Lazuli Gallery"
    H.div ! A.class_ "grid" $
      mapM_ cardHtml entries

cardHtml :: (String, String, Int, String) -> H.Html
cardHtml (styleName, paletteName, seed, thumbFile) =
  H.div ! A.class_ "card" $ do
    H.img ! A.src (H.toValue thumbFile)
    H.div ! A.class_ "info" $ do
      H.div $ H.toHtml $
        "Style: " ++ styleName ++ " | Palette: " ++ paletteName ++ " | Seed: " ++ show seed
      H.code $ H.toHtml $
        "lazuli --style " ++ styleName ++ " --palette " ++ paletteName ++ " --seed " ++ show seed

galleryCss :: String
galleryCss = unlines
  [ "body { background: #111; color: #eee; font-family: monospace; }"
  , ".grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(480px, 1fr)); gap: 16px; padding: 16px; }"
  , ".card { background: #222; border-radius: 8px; overflow: hidden; }"
  , ".card img { width: 100%; display: block; }"
  , ".card .info { padding: 8px; font-size: 12px; }"
  , ".card code { color: #0f0; }"
  ]
