# lazuli — A Haskell-Powered Generative Wallpaper Engine

## Project Summary

**lazuli** is a CLI tool that generates unique, high-resolution wallpapers from pure mathematical functions. Wallpapers are defined as compositions of noise functions, gradients, geometric patterns, and color palettes — all expressed as pure Haskell combinators. The user runs a command like:

```
lazuli --style voronoi --palette sunset --seed 42 --res 3840x2160 -o wallpaper.png
```

...and gets a beautiful, deterministic, one-of-a-kind wallpaper as a PNG.

The project is designed for a **2-day hackathon** (AmeriHac 2026). It's intended to be visually impressive, architecturally elegant, and a showcase for functional programming.

---

## Why This Project

- **Tangible output**: the deliverable is literal art. You can show wallpapers to anyone and they immediately understand what the tool does.
- **Deeply functional**: the entire architecture is built on function composition. An image is `(Double, Double) -> Color`. Styles are higher-order functions. Combinators compose fields. There is no mutable state.
- **Hackathon-friendly**: you can get a working MVP in a few hours and spend the rest of the time making it beautiful. Every hour of work produces visibly better output.
- **Useful**: people actually want unique wallpapers. A `--random` flag and a cron job means a new wallpaper every morning.

---

## Architecture

### Core Abstraction

The entire system is built on two types:

```haskell
-- A field maps 2D coordinates to a value
type Field a = (Double, Double) -> a

-- The two primary field types
type ScalarField = Field Double      -- values in [0, 1], used for noise, masks, distance fields
type ColorField  = Field Color       -- the final image: every pixel is a function call

-- Color representation (RGBA, 0-1 range)
data Color = Color
  { r :: !Double
  , g :: !Double
  , b :: !Double
  , a :: !Double
  }
```

A wallpaper is a `ColorField` — a pure function from `(x, y)` coordinates to a color. Rendering is just evaluating this function for every pixel.

### The Rendering Pipeline

```
User input (CLI args)
  │
  ├── Seed (Int)          ── deterministic randomness
  ├── Style (String)      ── selects a named composition
  ├── Palette (String)    ── selects a color ramp
  └── Resolution (W × H)  ── output dimensions
  │
  ▼
Style function: Seed -> Palette -> ColorField
  │
  ▼
Render: evaluate ColorField at every (x, y) pixel
  │  (embarrassingly parallel — each pixel is independent)
  │
  ▼
Write PNG via JuicyPixels
```

There is no intermediate representation, no scene graph, no retained state. The image *is* the function.

---

## Building Blocks

### 1. Scalar Fields (Raw Materials)

These are the primitives — functions that produce a `Double` value at every point in 2D space. They are the "textures" that get colored by palettes.

| Function | Description | Visual Character |
|---|---|---|
| `perlin seed freq` | Classic Perlin noise | Smooth, cloudy, organic |
| `simplex seed freq` | Simplex noise (fewer artifacts than Perlin) | Smooth, slightly sharper |
| `worley seed n` | Distance to Nth nearest random point | Cellular, bubbly |
| `voronoi seed n` | Which region you're in (nearest point ID) | Hard-edged cells |
| `fbm octaves seed freq` | Fractal Brownian Motion (layered noise) | Complex, natural terrain |
| `ridged seed freq` | Ridged multifractal noise | Mountain ridges, veins |
| `checkerboard freq` | Alternating 0/1 grid | Sharp geometric grid |
| `stripes freq angle` | Parallel stripes at an angle | Barcode-like bands |
| `ripple center freq` | Concentric rings from a point | Ripples in water |
| `spiral center arms` | Spiral arms from a center | Galaxy-like swirl |
| `radialGradient center` | Distance from center, normalized | Smooth circular falloff |
| `linearGradient angle` | Gradient along an axis | Smooth directional ramp |
| `mandelbrot center zoom` | Mandelbrot set escape time | Fractal (stretch goal) |

**Implementation priority**: `simplex`, `worley`/`voronoi`, `fbm`, `radialGradient`, `linearGradient`, `ripple`, `stripes` cover the vast majority of interesting compositions. Start with these.

### 2. Combinators (The Haskell Magic)

These are higher-order functions that transform and combine fields. This is where the architecture earns its Haskell stripes.

#### Field Transformations (ScalarField → ScalarField)

```haskell
-- Layer multiple octaves of noise for fractal detail
octaves :: Int -> Double -> ScalarField -> ScalarField

-- Clamp values to [0, 1]
clamp :: ScalarField -> ScalarField

-- Hard threshold: below t → 0, above t → 1
threshold :: Double -> ScalarField -> ScalarField

-- Smooth step between two thresholds
smoothstep :: Double -> Double -> ScalarField -> ScalarField

-- Invert: f(x,y) → 1 - f(x,y)
invert :: ScalarField -> ScalarField

-- Remap range: map [oldMin, oldMax] to [0, 1]
remap :: Double -> Double -> ScalarField -> ScalarField

-- Absolute value (creates ridged effects)
absField :: ScalarField -> ScalarField

-- Power: raise values to a power (contrast control)
power :: Double -> ScalarField -> ScalarField
```

#### Spatial Transformations (Field a → Field a)

```haskell
-- Affine transforms: modify the input coordinates
rotate :: Double -> Field a -> Field a
scale  :: Double -> Double -> Field a -> Field a  -- scaleX, scaleY
translate :: Double -> Double -> Field a -> Field a
tile :: Int -> Int -> Field a -> Field a  -- repeat in a grid

-- Mirror/reflect
mirrorX :: Field a -> Field a
mirrorY :: Field a -> Field a

-- Polar coordinates: convert (x, y) to (angle, radius) before sampling
toPolar :: Field a -> Field a
```

#### Domain Warping (The Star of the Show)

Domain warping is when you use noise to distort the *input coordinates* of another field. It produces the most visually stunning results — organic, fluid, alien-looking distortions.

```haskell
warp :: ScalarField    -- x-displacement field
     -> ScalarField    -- y-displacement field
     -> Double         -- warp strength
     -> Field a        -- field to warp
     -> Field a
warp dx dy strength f = \(x, y) ->
  let dx' = dx (x, y) * strength
      dy' = dy (x, y) * strength
  in f (x + dx', y + dy')
```

This single combinator is responsible for the most impressive visual output. Two layers of simplex noise warping a voronoi pattern creates something that looks like alien biology.

#### Blending / Composition (ColorField → ColorField → ColorField)

```haskell
-- Linear interpolation between two color fields
blend :: Double -> ColorField -> ColorField -> ColorField

-- Use a scalar field as a mask: 0 → field A, 1 → field B
mask :: ScalarField -> ColorField -> ColorField -> ColorField

-- Additive blending
add :: ColorField -> ColorField -> ColorField

-- Multiply (darkens)
multiply :: ColorField -> ColorField -> ColorField

-- Screen (lightens)
screen :: ColorField -> ColorField -> ColorField

-- Layer one field over another using alpha
over :: ColorField -> ColorField -> ColorField
```

### 3. Palettes (ScalarField → ColorField)

A palette maps a scalar value in [0, 1] to a color. This is the bridge between the grayscale math and the final colorful image.

```haskell
type Palette = Double -> Color

applyPalette :: Palette -> ScalarField -> ColorField
applyPalette pal field = \(x, y) -> pal (field (x, y))
```

#### Built-in Palettes

Ship with at least 8–10 named palettes:

| Name | Description | Vibe |
|---|---|---|
| `sunset` | Deep orange → magenta → dark purple | Warm, dramatic |
| `ocean` | Deep navy → teal → foam white | Cool, calming |
| `cyberpunk` | Black → hot pink → electric cyan | Neon, edgy |
| `moss` | Dark green → sage → cream | Earthy, natural |
| `fire` | Black → red → orange → bright yellow | Intense, hot |
| `ice` | White → pale blue → deep blue → black | Cold, clean |
| `vaporwave` | Pink → purple → teal → pink (cyclic) | Retro aesthetic |
| `monochrome` | Black → white | Classic, versatile |
| `infrared` | Black → purple → red → yellow → white | Thermal camera |
| `aurora` | Deep blue → green → teal → purple | Northern lights |

**Implementation**: each palette is a function that interpolates between a list of color stops. A helper function makes defining new palettes trivial:

```haskell
-- Define a palette from a list of (position, color) stops
fromStops :: [(Double, Color)] -> Palette
fromStops stops t = -- lerp between the two nearest stops
```

**Stretch**: import palettes from [lospec.com](https://lospec.com/palette-list) or from a user-provided image (extract dominant colors via k-means).

### 4. Styles (Named Compositions)

A style is a named function that wires together scalar fields, combinators, and a palette into a final `ColorField`. This is what the user selects with `--style`.

```haskell
type Style = Seed -> Palette -> ColorField
```

#### Example Styles

**Voronoi Cells** — warped voronoi with colored cells:
```haskell
voronoiStyle :: Style
voronoiStyle seed pal =
  applyPalette pal
    $ warp (simplex seed 0.003) (simplex (seed+1) 0.003) 80.0
    $ voronoi seed 30
```

**Nebula** — layered fractal noise blended with worley:
```haskell
nebulaStyle :: Style
nebulaStyle seed pal =
  blend 0.6
    (applyPalette pal $ fbm 6 seed 0.002)
    (applyPalette pal $ warp n1 n2 60.0 $ worley seed 20)
  where
    n1 = simplex (seed+1) 0.004
    n2 = simplex (seed+2) 0.004
```

**Crystal** — ridged noise with hard edges:
```haskell
crystalStyle :: Style
crystalStyle seed pal =
  applyPalette pal
    $ power 1.5
    $ ridged seed 0.005
```

**Domain Warp** — pure warping showcase, the trippiest output:
```haskell
domainWarpStyle :: Style
domainWarpStyle seed pal =
  applyPalette pal
    $ warp l2x l2y 120.0  -- second layer of warping
    $ warp l1x l1y 80.0   -- first layer of warping
    $ simplex seed 0.003  -- base pattern
  where
    l1x = simplex (seed+1) 0.004
    l1y = simplex (seed+2) 0.004
    l2x = simplex (seed+3) 0.003
    l2y = simplex (seed+4) 0.003
```

**Flow** — noise-driven flow field visualization:
```haskell
flowStyle :: Style
flowStyle seed pal =
  applyPalette pal
    $ mask (radialGradient (0.5, 0.5))
      (fbm 4 seed 0.005)
      (warp (spiral (0.5, 0.5) 5) (simplex seed 0.003) 40.0
        $ stripes 0.01 (pi/4))
```

**Stained Glass** — voronoi cells with distinct border lines:
```haskell
stainedGlassStyle :: Style
stainedGlassStyle seed pal =
  mask borders
    (constColor black)
    (applyPalette pal $ voronoi seed 40)
  where
    borders = threshold 0.02 $ worley seed 40  -- thin lines at cell boundaries
```

**Target**: ship with 6–8 styles. Each should produce visually distinct output.

---

## CLI Interface

Built with `optparse-applicative`.

### Basic Usage

```bash
# Generate a single wallpaper
lazuli --style voronoi --palette sunset --seed 42 --res 3840x2160 -o wallpaper.png

# Random everything
lazuli --random -o surprise.png

# Quick preview at low resolution
lazuli --style nebula --palette cyberpunk --seed 7 --preview

# Gallery mode: generate N random wallpapers as a thumbnail grid
lazuli --gallery 20 -o gallery.html

# List available styles and palettes
lazuli --list-styles
lazuli --list-palettes
```

### Full CLI Options

```
lazuli - Generative wallpaper engine

Usage: lazuli [OPTIONS]

Options:
  -s, --style STYLE        Style name (voronoi, nebula, crystal, domainwarp,
                            flow, stainedglass, ...) [default: random]
  -p, --palette PALETTE    Palette name (sunset, ocean, cyberpunk, moss,
                            fire, ice, vaporwave, ...) [default: random]
  -S, --seed INT           Random seed for deterministic generation
                            [default: random]
  -r, --res WxH            Output resolution [default: 1920x1080]
  -o, --output FILE        Output file path [default: lazuli-{seed}.png]
      --preview            Render at 1/4 resolution for quick preview
      --random             Randomize style, palette, and seed
      --gallery N          Generate N random wallpapers as an HTML gallery
      --list-styles        List available styles with descriptions
      --list-palettes      List available palettes with color previews
      --set-wallpaper      Set the generated image as desktop wallpaper
                            (calls feh, gsettings, or osascript depending on OS)
  -j, --jobs N             Parallel rendering threads [default: auto-detect]
  -h, --help               Show help
      --version            Show version
```

### The `--gallery` Flag (Hackathon Demo Killer)

This generates an HTML page with a grid of thumbnail wallpapers. Each thumbnail links to the full command to reproduce it:

```html
<!-- Each thumbnail shows: style name, palette name, seed -->
<!-- Clicking opens the full-res version -->
<!-- The full CLI command is shown below each thumbnail for reproducibility -->
```

This is the single most important feature for demo day. You run `lazuli --gallery 20`, open the HTML file, and you have an instant art exhibition.

---

## Project Structure

```
lazuli/
├── app/
│   └── Main.hs                 -- CLI entry point (optparse-applicative)
├── src/
│   └── Lazuli/
│       ├── Types.hs            -- Color, Field, ScalarField, ColorField
│       ├── Noise.hs            -- Perlin, simplex, worley, voronoi
│       ├── Patterns.hs         -- Geometric: stripes, checkerboard, ripple, spiral, gradients
│       ├── Combinators.hs      -- blend, mask, warp, tile, rotate, scale, threshold, etc.
│       ├── Palette.hs          -- Palette type, fromStops, all named palettes
│       ├── Style.hs            -- Style type, all named styles, style registry
│       ├── Render.hs           -- Parallel pixel evaluation, PNG output via JuicyPixels
│       └── Gallery.hs          -- HTML gallery generation
├── palettes/                   -- Optional: palette definitions as YAML/JSON for easy editing
├── lazuli.cabal
├── README.md
└── examples/                   -- Pre-generated example wallpapers for the README
```

### Module Dependency Graph

```
Main.hs
  └── Style.hs (selects the composition)
        ├── Combinators.hs (warp, blend, mask, ...)
        │     └── Types.hs
        ├── Noise.hs (simplex, worley, ...)
        │     └── Types.hs
        ├── Patterns.hs (gradients, stripes, ...)
        │     └── Types.hs
        └── Palette.hs (color ramps)
              └── Types.hs
  └── Render.hs (parallel pixel eval + PNG write)
        └── Types.hs
  └── Gallery.hs (HTML output for gallery mode)
```

---

## Dependencies

| Package | Purpose | Notes |
|---|---|---|
| `JuicyPixels` | PNG image output | Mature, zero-hassle |
| `optparse-applicative` | CLI argument parsing | Standard Haskell CLI library |
| `vector` | Pixel buffer for rendering | Performance-critical inner loop |
| `random` / `splitmix` | Seeded random number generation | Deterministic output from seed |
| `parallel` / `deepseq` | Parallel pixel evaluation | Each pixel is independent |
| `blaze-html` | HTML generation for gallery mode | Only needed for `--gallery` |
| `time` | Timestamp-based default seeds | Minimal use |

No exotic dependencies. Everything is on Stackage.

---

## Implementation Plan (2-Day Hackathon)

### Day 1 — Morning (4 hours): Core Engine

**Goal**: `lazuli --style simplex --palette monochrome --seed 42 --res 800x600 -o test.png` works and produces a visible image.

1. **Types.hs** — define `Color`, `Field`, `ScalarField`, `ColorField` (~15 min)
2. **Noise.hs** — implement simplex noise from scratch or port a known algorithm. This is the hardest single piece. (~2 hours)
   - Alternative: use a Haskell noise library if one exists and is adequate. Check Hackage for `opensimplex`, `noiselib`, etc. If none are good, implement simplex noise. It's well-documented mathematically.
3. **Palette.hs** — implement `fromStops` interpolation and 3 palettes: `monochrome`, `sunset`, `cyberpunk` (~30 min)
4. **Render.hs** — evaluate a `ColorField` at every pixel, write PNG via JuicyPixels (~45 min)
5. **Main.hs** — minimal CLI with `--seed`, `--res`, `--output` (~30 min)

**Milestone check**: you can generate a grayscale simplex noise image. It's not pretty yet, but the pipeline works end to end.

### Day 1 — Afternoon (4 hours): Combinators + Styles

**Goal**: 4 distinct styles producing genuinely beautiful wallpapers.

1. **Noise.hs** — add `worley` / `voronoi` distance (~1 hour)
2. **Patterns.hs** — add `radialGradient`, `linearGradient`, `stripes`, `ripple` (~45 min)
3. **Combinators.hs** — implement `warp`, `blend`, `mask`, `threshold`, `rotate`, `scale`, `octaves`/`fbm` (~1.5 hours)
4. **Style.hs** — compose 4 named styles: `voronoi`, `nebula`, `domainwarp`, `crystal` (~30 min)
5. **Palette.hs** — add remaining palettes to reach 8 total (~15 min)

**Milestone check**: `lazuli --style domainwarp --palette cyberpunk --seed 42 --res 1920x1080` produces a stunning wallpaper. This is the moment you know you have a good project.

### Day 2 — Morning (4 hours): Polish + Gallery

**Goal**: the tool is pleasant to use and the gallery mode works.

1. **CLI polish** — add `--random`, `--preview`, `--list-styles`, `--list-palettes`, `--style` and `--palette` flags with proper defaults (~1 hour)
2. **Gallery.hs** — implement `--gallery N`: render N wallpapers at thumbnail resolution, output an HTML grid page (~1.5 hours)
3. **Performance** — add parallel rendering with `parMap` or `parVector`. On a 4-core machine, 4x speedup on large renders. (~45 min)
4. **Style.hs** — add 2-3 more styles: `stainedglass`, `flow`, `aurora` (~45 min)

**Milestone check**: `lazuli --gallery 20` produces a gorgeous HTML page with 20 unique wallpapers.

### Day 2 — Afternoon (4 hours): Stretch Goals + Demo Prep

Pick 1-2 stretch goals from the list below, then prepare for demo.

1. **Stretch goal work** (~2 hours)
2. **Generate example wallpapers** for the README at full resolution (~30 min)
3. **Write README** with usage examples, example images, and architecture overview (~30 min)
4. **Demo prep** — generate the gallery, pick the best wallpapers, prepare a 3-minute walkthrough (~30 min)

---

## Stretch Goals (Pick 1-2)

### A. Animation Mode
Interpolate a parameter (seed drift, warp strength, rotation) over time. Output frames or a GIF.

```bash
lazuli --style domainwarp --palette sunset --seed 42 --animate --frames 120 --fps 30 -o anim.gif
```

Implementation: render each frame as a `ColorField` where one parameter varies with frame number. Use JuicyPixels or `ffmpeg` for GIF/video assembly. This produces mesmerizing looping animations.

### B. Live Preview Web UI
A local web server (scotty) that serves a low-res preview and has controls for style, palette, seed, and warp parameters. Every change re-renders and pushes the new image to the browser.

```bash
lazuli --live --port 8080
```

Opens `localhost:8080` with sliders and dropdowns. Great for exploration and demo.

### C. --set-wallpaper Flag
Detect the OS/DE and set the wallpaper automatically:
- Linux/GNOME: `gsettings set org.gnome.desktop.background picture-uri file://...`
- Linux/i3/sway: `feh --bg-fill ...` or `swaybg`
- macOS: `osascript -e 'tell application "Finder" to set desktop picture to POSIX file "..."'`

Small feature, big quality-of-life impact.

### D. Palette from Image
Accept a `--palette-from image.jpg` flag. Extract dominant colors via k-means clustering (5-7 colors), sort by luminance, and use as a palette.

```bash
lazuli --style nebula --palette-from my-photo.jpg --seed 42
```

"Make me a wallpaper that matches the colors of this sunset photo I took."

### E. Config File for Daily Wallpapers
A `~/.config/lazuli/config.toml`:

```toml
[daily]
styles = ["voronoi", "nebula", "domainwarp"]
palettes = ["sunset", "ocean", "cyberpunk"]
resolution = "3840x2160"
output_dir = "~/.wallpapers/"
set_wallpaper = true
```

Pair with a systemd timer or cron job. Wake up to a new unique wallpaper every day.

### F. Multi-Monitor Support
```bash
lazuli --monitors 2 --layout side-by-side --res 3840x2160+3840x2160
```

Generates a single ultra-wide image where the composition is aware of the monitor split — e.g., a gradient that flows naturally across both screens rather than being awkwardly cropped.

---

## Technical Notes

### Noise Implementation

Simplex noise is the most important building block. Options:

1. **Port from a reference implementation** — there are well-documented C/Java/Python implementations. Translating to Haskell is straightforward. Ken Perlin's improved noise and the simplex noise algorithm are both well-published.
2. **Use a Haskell library** — check Hackage for `opensimplex2`, `noiselib`, or similar. If one works and is performant, use it. Don't reinvent for the sake of it.
3. **Write your own** — if you want the full learning experience. Simplex noise in 2D requires: a skew/unskew function, a gradient table, hashing of grid coordinates. It's ~50 lines of code but tricky to get right.

Worley/Voronoi noise: generate random points in a grid, for each pixel find the distance to the nearest N points. Straightforward to implement.

### Performance

At 4K resolution (3840 × 2160 = 8.3 million pixels), every pixel requires evaluating the full composition function (which may involve multiple noise lookups). Key performance strategies:

- **Parallelism**: pixels are independent. Use `Control.Parallel.Strategies` to evaluate rows or chunks in parallel. This is nearly free in Haskell.
- **Strict evaluation**: use strict `Double` fields in `Color`, use `Data.Vector.Unboxed` for the pixel buffer.
- **Noise lookup caching**: if the same noise field is sampled multiple times in a composition, consider caching. Usually not needed at hackathon scope.
- **Preview mode**: render at 1/4 resolution (960×540) for quick iteration. The structure is visible even at low res.

**Expected render times** (rough estimates, single core):
- 1080p: 2-5 seconds for simple styles, 10-20 seconds for complex (multi-octave, multi-warp)
- 4K: 8-20 seconds simple, 40-80 seconds complex
- With 4-core parallelism: divide by ~3.5

These are fine for a CLI tool. The `--preview` flag handles the iteration speed concern.

### Coordinate System

Normalize coordinates to [0, 1] × [0, 1] regardless of resolution. This means styles and patterns are resolution-independent — the same seed produces the same *composition* at any resolution, just with more or less detail.

```haskell
render :: Int -> Int -> ColorField -> Image PixelRGBA8
render w h field = generateImage pixelAt w h
  where
    pixelAt x y =
      let nx = fromIntegral x / fromIntegral w
          ny = fromIntegral y / fromIntegral h
          Color r g b a = field (nx, ny)
      in PixelRGBA8 (to8 r) (to8 g) (to8 b) (to8 a)
    to8 v = round (clamp01 v * 255)
```

### Determinism

Every generated wallpaper must be fully deterministic given the same `(style, palette, seed, resolution)` tuple. This means:
- All randomness flows from the seed
- No use of `randomIO` or system time in the generation pipeline
- The same command always produces the exact same image

This is critical for reproducibility — the gallery can show the exact command to recreate any wallpaper.

---

## What to Show at the Demo

1. **The gallery** — `lazuli --gallery 20` opened in a browser. 20 unique wallpapers in a grid. Instant visual impact.
2. **Live generation** — run a few `lazuli` commands live, show wallpapers appearing in a few seconds.
3. **The code** — show a style definition side-by-side with its output. "This is the entire program that generated this image" is a powerful moment.
4. **Set as wallpaper** — `lazuli --random --set-wallpaper` and watch the desktop change live.
5. **Reproducibility** — "every image has a seed, here's how to get it back: `lazuli --style nebula --palette sunset --seed 42`" — run it, same image appears.

---

## References and Inspiration

- **Inigo Quilez** — [https://iquilezles.org/articles/](https://iquilezles.org/articles/) — the gold standard for noise functions, distance fields, and domain warping. His articles on `warp`, `fbm`, and `voronoi` are essential reading.
- **The Book of Shaders** — [https://thebookofshaders.com/](https://thebookofshaders.com/) — chapters on noise, fractals, and generative patterns translate directly to this project (they're written for GLSL but the math is identical).
- **Lospec Palette List** — [https://lospec.com/palette-list](https://lospec.com/palette-list) — source of curated color palettes.
- **Red Blob Games on Noise** — [https://www.redblobgames.com/articles/noise/introduction.html](https://www.redblobgames.com/articles/noise/introduction.html) — excellent interactive explanation of Perlin/simplex noise.
- **Adrian's Soapbox on Domain Warping** — search "domain warping inigo quilez" — the technique that produces the most impressive visual results.

---

## Team Size and Skill Recommendations

- **1-2 people**: totally viable. One person can build the core engine + CLI. Two people can split: one on noise/math, one on CLI/gallery/palettes.
- **3 people**: one on noise + combinators, one on styles + palettes, one on CLI + gallery + stretch goals.
- **Haskell skill level**: intermediate. You should be comfortable with higher-order functions, type aliases, basic IO, and `Data.Vector`. No advanced type-level programming, no monad transformers, no lens (unless you want them).
- **Math skill level**: basic. You need to understand what "map (x,y) to a value" means, and the concept of interpolation. The noise algorithms can be ported from reference implementations without deep understanding of the math.
