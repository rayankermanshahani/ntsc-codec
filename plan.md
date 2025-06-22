# NTSC Codec Development Plan

## Overview
This document outlines the implementation plan for completing the NTSC codec, building upon the existing type system and CLI infrastructure. Each component will be implemented as a separate module, maintaining type safety and functional purity throughout.

## 1. Signal Generation Module (`NTSC.Signal`)

### 1.1 Core Signal Generators
```haskell
module NTSC.Signal where
```

**Key Functions:**
- `generateSyncPulse :: SignalParams -> SyncType -> Time -> [Sample]`
  - Pure function generating samples for horizontal/vertical sync pulses
  - Use continuation-passing style for efficiency with large sample lists
  
- `generateColorBurst :: Frequency -> Phase -> IRELevel -> Time -> Frequency -> [Sample]`
  - Generate 8-10 cycles of subcarrier at specific phase
  - Implement phase accumulator using `State` monad for clarity
  
- `generateBlankingLevel :: SignalParams -> Time -> Frequency -> [Sample]`
  - Simple constant-level signal generation
  
- `assembleHorizontalLine :: NTSCConfig -> [Pixel] -> [Sample]`
  - Compose complete line with proper timing segments
  - Use `Writer` monad to accumulate samples efficiently

### 1.2 Composite Signal Assembly
- `modulateChroma :: Frequency -> Time -> YIQ -> [Sample]`
  - Quadrature amplitude modulation of I/Q signals
  - Pre-compute sine/cosine tables for performance
  
- `combineSignals :: LuminanceSignal -> ChrominanceSignal -> CompositeSignal`
  - Add luminance and modulated chrominance
  - Implement using `zipWith` for parallelization potential

**Implementation Notes:**
- Use `Data.Vector.Unboxed` for sample storage (better cache locality)
- Implement lazy streaming where possible to handle large video files
- Create QuickCheck properties for signal timing invariants

## 2. I/O Modules

### 2.1 Image I/O (`NTSC.IO.Image`)
```haskell
module NTSC.IO.Image where
```

**Key Types:**
```haskell
data ImageSequence = ImageSequence
  { isFrames :: Vector Frame
  , isMetadata :: ImageMetadata
  }

class ImageFormat a where
  readImage :: FilePath -> IO (Either IOError a)
  writeImage :: FilePath -> a -> IO (Either IOError ())
```

**Implementations:**
- PPM format (simple, no external dependencies)
  - Parser using `attoparsec` for efficiency
  - Writer using `ByteString.Builder`
  
- PNG format (using `JuicyPixels` library)
  - Add to dependencies: `JuicyPixels ^>= 3.3`
  
- Raw formats (RGB/YUV)
  - Memory-mapped I/O using `mmap` package for large files
  - Configurable pixel formats and byte ordering

### 2.2 Signal I/O (`NTSC.IO.Signal`)
```haskell
module NTSC.IO.Signal where
```

**Key Functions:**
- `readCompositeSignal :: FilePath -> IO (Either IOError CompositeSignal)`
  - Support both float32 and int16 sample formats
  - Header with sample rate and format information
  
- `writeCompositeSignal :: FilePath -> CompositeSignal -> IO (Either IOError ())`
  - Streaming write to handle large files
  - Optional compression using `zlib`

**File Format Design:**
```
Header (64 bytes):
  - Magic number: "NTSC" (4 bytes)
  - Version: 1 (4 bytes)
  - Sample rate (8 bytes, double)
  - Sample format (4 bytes, enum)
  - Frame count (4 bytes)
  - Reserved (40 bytes)
Data:
  - Raw samples in specified format
```

## 3. Modulation/Demodulation (`NTSC.Modulation`)

### 3.1 Modulator
```haskell
module NTSC.Modulation where
```

**Key Functions:**
- `encodeFrame :: NTSCConfig -> Frame -> CompositeSignal`
  - Process fields in correct order for interlacing
  - Handle half-line offset for proper interlace
  
- `encodeField :: NTSCConfig -> Field -> [Sample]`
  - Generate vertical blanking interval with proper sync patterns
  - Encode each line with correct timing

### 3.2 Demodulator
- `decodeComposite :: NTSCConfig -> CompositeSignal -> IO Frame`
  - Sync detection using edge detection and PLL
  - Implement in `StateT IO` for maintaining decoder state
  
- `syncDetector :: [Sample] -> StateT DecoderState IO SyncInfo`
  - Adaptive threshold for sync detection
  - Phase-locked loop for subcarrier recovery
  
- `chromaDemodulator :: Frequency -> Phase -> [Sample] -> (YIQ, DecoderState)`
  - Quadrature demodulation with I/Q extraction
  - Maintain phase continuity across lines

**Decoder State:**
```haskell
data DecoderState = DecoderState
  { dsPhaseAccumulator :: !Phase
  , dsSyncThreshold :: !Sample
  , dsLineCount :: !Int
  , dsFieldType :: !FieldType
  , dsColorBurstPhase :: !Phase
  }
```

## 4. Digital Filters (`NTSC.Filter`)

### 4.1 Filter Types
```haskell
module NTSC.Filter where

-- Use type-safe filter specifications
data FilterSpec
  = LowPass { fcCutoff :: Frequency, fcOrder :: Int }
  | BandPass { fcCenter :: Frequency, fcBandwidth :: Frequency }
  | CombFilter { fcDelay :: Int, fcFeedback :: Double }
```

### 4.2 Implementations
- **FIR Filters** (using `vector-fftw` for convolution)
  ```haskell
  applyFIR :: Vector Double -> Vector Sample -> Vector Sample
  ```
  
- **IIR Filters** (using direct form II for stability)
  ```haskell
  applyIIR :: FilterCoeffs -> State FilterState [Sample] -> [Sample]
  ```
  
- **Comb Filter** (for chroma/luma separation)
  ```haskell
  combFilter :: Int -> [Sample] -> ([Sample], [Sample])
  ```

**Design Principles:**
- Pre-compute filter coefficients at configuration time
- Use SIMD operations where available (via `vector` library)
- Implement both streaming and batch processing variants

## 5. Frame Assembly/Disassembly (`NTSC.Frame`)

### 5.1 Frame Construction
```haskell
module NTSC.Frame where
```

**Key Functions:**
- `pixelsToFrame :: NTSCConfig -> [[Pixel]] -> Frame`
  - Handle both progressive and interlaced input
  - Proper field ordering for interlace
  
- `deinterlace :: Frame -> Frame`
  - Multiple algorithms: bob, weave, motion-adaptive
  - Pure functional implementation using field interpolation

### 5.2 Pixel Processing Pipeline
```haskell
-- Type-safe pipeline using Arrows
frameProcessor :: NTSCConfig -> FrameProcessor RGB Frame
frameProcessor cfg = proc pixels -> do
  gamma <- gammaCorrector (cfgGamma cfg) -< pixels
  yiq <- colorConverter rgbToYiq -< gamma
  limited <- bandwidthLimiter cfg -< yiq
  returnA -< assembleFrame cfg limited
```

## Testing Strategy

### Property-Based Testing
- Signal timing properties (line duration, sync pulse widths)
- Roundtrip encoding/decoding preserves key image features
- Filter frequency response characteristics

### Benchmarking Suite
```haskell
module Bench.NTSC where
-- Using criterion
benchmarks = bgroup "NTSC"
  [ bench "encode frame" $ nf (encodeFrame cfg) testFrame
  , bench "comb filter" $ nf (combFilter 2) testSignal
  , bench "color conversion" $ nf (map rgbToYiq) testPixels
  ]
```

## Performance Considerations

1. **Parallelization**
   - Use `parallel` library for line-level parallelism
   - Process fields concurrently where possible

2. **Memory Usage**
   - Stream processing for large files
   - Strict fields in data types to avoid thunks
   - Unboxed vectors for numeric data

3. **Optimization Flags**
   ```cabal
   ghc-options: -O2 
                -funbox-strict-fields
                -fspecialise-aggressively
                -flate-dmd-anal
   ```

## Module Dependency Graph
```
NTSC.Types ─┬─> NTSC.Color
            ├─> NTSC.Signal ───> NTSC.Modulation
            ├─> NTSC.Filter ───┘
            ├─> NTSC.Frame
            └─> NTSC.IO.* ─────> CLI
```

## Implementation Order

1. **Phase 1**: Signal Generation
   - Basic signal generators
   - Composite signal assembly
   - Unit tests for timing

2. **Phase 2**: Basic I/O
   - PPM format support
   - Raw signal I/O
   - Integration tests

3. **Phase 3**: Modulation Core
   - Frame encoder
   - Sync detector
   - Basic demodulator

4. **Phase 4**: Filters
   - Low-pass and band-pass
   - Comb filter
   - Performance optimization

5. **Phase 5**: Polish
   - Additional formats
   - Performance tuning
   - Documentation

## Code Quality Standards

- **Type Safety**: Leverage Haskell's type system to prevent runtime errors
- **Purity**: Keep I/O at the boundaries, pure functions for signal processing
- **Documentation**: Haddock comments for all exported functions
- **Testing**: Minimum 80% code coverage, property tests for invariants
- **Performance**: Profile-guided optimization, space leak detection