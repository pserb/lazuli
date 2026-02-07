# Postprocessing Shaders for Lazuli

## Overview
Add a postprocessing pipeline that runs **after** the field is rendered to an image but **before** PNG encoding. This allows image-based effects (blur, bloom, etc.) rather than field-based effects (vignette, smoothstep).

## Data Flow Change

```
Current: ColorField → Image → PNG
New:     ColorField → Image → [Effect] → Image → PNG
```

## Effects to Implement

| Effect | Description | Parameters |
|--------|-------------|------------|
| **gaussianBlur** | Soft blur | radius (px), sigma |
| **sharpen** | Edge enhancement | amount |
| **bloom** | Glow effect on bright areas | threshold, intensity, radius |
| **vignette** | Edge darkening (image-based version) | radius, softness |
| **contrast** | Contrast adjustment | factor |
| **saturation** | Color saturation | factor |
| **brightness** | Brightness adjustment | offset |
| **sepia** | Sepia tone | amount |
| **grayscale** | Desaturate | method (avg, luma, desat) |
| **pixelate** | Blocky pixels | blockSize |
| **dither** | Add noise for reduced banding | amount |
| **noise** | Add grain | amount, size |
| **distort** | Simple radial distortion | amount, center |
| **flutedGlass** | Frosted glass effect with vertical ridges | amount, ridgeWidth |
| **halftone** | Dotted halftone pattern (newsprint style) | dotSize, frequency |
| **water** | Wavy water distortion effect | amplitude, frequency |
| **paperTexture** | Paper grain/texture overlay | intensity, scale |
| **crackle** | Cracked paint effect | amount |
| **marble** | Marble veining effect | intensity, scale |
| **wood** | Wood grain effect | intensity, grainWidth |
| **plastic** | Glossy plastic look | specularAmount, colorTint |
| **metal** | Brushed metal look | intensity, direction |
| **glass** | Clear glass distortion | refractionIndex |
| **oilPaint** | Oil painting brush stroke effect | brushSize, colorCount |
| **charcoal** | Charcoal sketch effect | intensity |
| **neon** | Neon glow outline effect | threshold, glowColor |
| **ink** | Ink illustration effect | lineIntensity |

## Implementation Strategy

### 1. Effect Type (`src/Lazuli/Effect.hs`)
```haskell
type Effect = Image PixelRGBA8 -> Image PixelRGBA8

applyEffects :: [Effect] -> Image PixelRGBA8 -> Image PixelRGBA8
applyEffects = foldl' (flip id)
```

### 2. Render Pipeline (`src/Lazuli/Render.hs`)
- Add `effects :: [Effect]` parameter to `render` and `renderToFileAA`
- Apply effects after pixel generation, before sRGB conversion

### 3. CLI Integration (`app/Main.hs`)
- Add `optEffect` to `Options` (multiple allowed)
- Parse effects from command line (e.g., `--effect blur:5 --effect bloom:0.5`)

### 4. Effects Library (`src/Lazuli/Effects.hs`)
- Implement each effect using JuicyPixels image operations
- Use convolution for blur/sharpen
- Use pixel manipulation for color adjustments

## Command Examples

```
lazuli --gallery 10 --effect blur:3 --effect saturation:1.2
lazuli --style cells --effect bloom:0.8 --effect noise:0.1
lazuli --effect grayscale --effect pixelate:4 --effect vignette:0.5
lazuli --effect contrast:1.5 --effect brightness:0.1

## Special Effects Examples

```
lazuli --effect flutedGlass:0.3:10 --effect blur:2
lazuli --effect halftone:3:8
lazuli --effect water:0.05:0.2
lazuli --effect paperTexture:0.3:2
lazuli --effect oilPaint:5:8
lazuli --effect marble:0.5:0.1
lazuli --effect plastic:0.7:255:0:0
lazuli --effect metal:0.6:horizontal
lazuli --effect neon:0.5:0:255:0
```

## Special Effects Implementation Details

### Fluted Glass
- Generate vertical sine-wave displacement map
- Apply displacement to source image
- Add subtle noise for frosted texture
- Parameters: `amount` (displacement strength), `ridgeWidth` (spacing)

### Halftone
- Sample image at grid intervals
- Convert brightness to dot size
- Create circular dot pattern
- Parameters: `dotSize` (max radius), `frequency` (grid density)

### Water
- Apply time-based sine displacement
- Create wave pattern with multiple frequencies
- Parameters: `amplitude` (wave height), `frequency` (wave spacing)

### Paper Texture
- Generate or sample noise texture
- Overlay on image with low opacity
- Can use seeded noise for consistency
- Parameters: `intensity` (opacity), `scale` (noise size)

### Dithering (Ordered)
- Bayer matrix or random noise
- Applied before quantization
- Reduces banding in gradients
- Parameters: `amount` (noise strength)

### Crackle
- Perlin noise-based crack pattern
- Apply with threshold for cracks
- Parameters: `amount` (crack density)

### Marble
- Sine-based warping with multiple octaves
- Simulates veining in stone
- Parameters: `intensity` (warp strength), `scale` (vein size)

### Wood
- Concentric ring pattern with noise
- Simulates wood grain
- Parameters: `intensity`, `grainWidth`

### Plastic
- Specular highlight overlay
- Slight color tint
- Parameters: `specularAmount`, `colorTint` (R:G:B)

### Metal
- Directional noise/lines
- Reflective overlay
- Parameters: `intensity`, `direction` (horizontal/vertical/diagonal)

### Glass
- Subtle displacement
- High contrast edges
- Parameters: `refractionIndex`

### Oil Paint
- Adaptive kernel painting
- Replace pixels with median color in region
- Parameters: `brushSize`, `colorCount` (posterization)

### Charcoal
- Edge detection + tonal reversal
- Simulates drawn charcoal
- Parameters: `intensity`

### Neon
- Threshold for bright areas
- Glow blur around bright areas
- Parameters: `threshold`, `glowColor` (R:G:B)

### Ink
- Edge detection with threshold
- Clean line art output
- Parameters: `lineIntensity`

## Technical Details

### Convolution Filter (for blur/sharpen)
JuicyPixels provides `filterImage` for applying convolution kernels:

```haskell
-- Gaussian blur kernel (3x3 example)
gaussianKernel3x3 :: [Double]
gaussianKernel3x3 = [1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9]
```

### Color Space Considerations
- Effects like saturation/contrast work best in linear space
- Convert to sRGB for display, apply effects to linear values
- Use `sRGBToLinear` and `linearToSRGB` conversions

### Performance Considerations
- Effects are applied once per image (not per pixel)
- Large blur radii may be expensive - consider separable convolutions
- For gallery mode, effects apply to each thumbnail

## Dependencies
JuicyPixels already provides:
- `filterImage` for convolution
- `convertImage` for color conversions
- `generateImage` / `extractPixels` / `pixelAt` for pixel manipulation
