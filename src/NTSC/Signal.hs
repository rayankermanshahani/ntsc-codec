{-# LANGUAGE RecordWildCards #-}

module NTSC.Signal
  ( -- * Basic Signal Generators
    generateSyncPulse,
    generateColorBurst,
    generateBlankingLevel,
    generateActiveVideo,

    -- * Composite Signal Assembly
    assembleHorizontalLine,
    modulateChroma,
    combineSignals,

    -- * Phase Management
    PhaseAccumulator (..),
    initPhaseAccumulator,
    advancePhase,

    -- * Utility Functions
    generateSineWave,
    generateCosineWave,
    resampleSignal,
  )
where

import Control.Monad (forM)
import Control.Monad.State.Strict
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import NTSC.Color
import NTSC.Types
import Prelude hiding (zipWith3)

-- | Phase accumulator
data PhaseAccumulator = PhaseAccumulator
  { -- | Current phase
    paPhase :: !Phase,
    -- | Oscillator frequency
    paFrequency :: !Frequency,
    -- | Sample rate
    paSampleRate :: !Frequency
  }
  deriving (Show, Eq)

-- | Initialize a phase accumulator
initPhaseAccumulator :: Frequency -> Frequency -> Phase -> PhaseAccumulator
initPhaseAccumulator freq sampleRate initialPhase =
  PhaseAccumulator
    { paPhase = initialPhase,
      paFrequency = freq,
      paSampleRate = sampleRate
    }

-- | Advance phase by one sample period
advancePhase :: PhaseAccumulator -> PhaseAccumulator
advancePhase pa@PhaseAccumulator {..} =
  pa
    { paPhase = Phase $ (unPhase paPhase + phaseIncrement) `mod'` (2 * pi)
    }
  where
    phaseIncrement = 2 * pi * unFrequency paFrequency / unFrequency paSampleRate
    mod' x y = x - y * fromIntegral (floor (x / y) :: Int)

-- | Generate sync pulse samples
generateSyncPulse :: SignalParams -> SyncType -> Time -> Frequency -> Vector Sample
generateSyncPulse SignalParams {..} syncType duration sampleRate =
  VU.replicate numSamples (Sample $ ireToVoltage syncLevel)
  where
    numSamples = secondsToSamples sampleRate duration
    syncLevel = case syncType of
      HorizontalSync -> spSyncLevel
      VerticalSync -> spBlankingLevel -- vertical sync goes to blanking level
      EqualizingPulse -> spSyncLevel

-- | Generate color burst signal
generateColorBurst :: ColorBurst -> Frequency -> Vector Sample
generateColorBurst ColorBurst {..} sampleRate =
  VU.generate numSamples generateSample
  where
    numSamples = secondsToSamples sampleRate burstDuration
    phaseStep = 2 * pi * unFrequency burstFrequency / unFrequency sampleRate
    amplitude = ireToVoltage burstAmplitude / 140.0 -- ±20 IRE normalized
    generateSample i =
      let phase = unPhase burstPhase + fromIntegral i * phaseStep
          -- NTSC burst is 180 deg out of phase with reference
          value = amplitude * sin (phase + pi)
       in Sample value

-- | Generate blanking level signal
generateBlankingLevel :: SignalParams -> Time -> Frequency -> Vector Sample
generateBlankingLevel SignalParams {..} duration sampleRate =
  VU.replicate numSamples (Sample $ ireToVoltage spBlankingLevel)
  where
    numSamples = secondsToSamples sampleRate duration

-- | Generate active video signal from pixels with chroma modulation
generateActiveVideo :: NTSCConfig -> [Pixel] -> Time -> State PhaseAccumulator (Vector Sample)
generateActiveVideo cfg@NTSCConfig {..} pixels duration = do
  let sampleRate = tpSampleRate cfgTiming
      numSamples = secondsToSamples sampleRate duration
      samplesPerPixel = numSamples `div` length pixels

  -- generate samples for each pixel
  samples <- forM (zip [0 ..] pixels) $ \(pixIdx, pixel) -> do
    let pixelSamples = min samplesPerPixel (numSamples - pixIdx * samplesPerPixel)
    generatePixelSamples cfg pixel pixelSamples

  return $ VU.concat samples

-- | Generate samples for a single pixel
generatePixelSamples :: NTSCConfig -> Pixel -> Int -> State PhaseAccumulator (Vector Sample)
generatePixelSamples NTSCConfig {..} Pixel {..} numSamples = do
  -- convert pixel to YIQ
  let yiq = rgbToYiq (gammaCorrect cfgGamma pixelColor)
      baseLevel = ireToVoltage (spBlackLevel cfgSignal) + (ireToVoltage (spWhiteLevel cfgSignal) - ireToVoltage (spBlackLevel cfgSignal)) * yiqY yiq

  -- generate chroma samples
  VU.generateM numSamples $
    \_ -> do
      pa <- get
      let i = yiqI yiq
          q = yiqQ yiq
          phase = unPhase (paPhase pa)
          -- Quadrature modulation: I*cos(ωt) + Q*sin(ωt)
          chroma = i * cos phase - q * sin phase
          -- scale chroma to prevent over-modulation
          chromaScaled = chroma * 0.3 -- ~ ±20 IRE for full saturation
      put (advancePhase pa)
      return $ Sample (baseLevel + chromaScaled)

-- | Assemble a complete horizontal line
assembleHorizontalLine :: NTSCConfig -> [Pixel] -> State PhaseAccumulator (Vector Sample)
assembleHorizontalLine cfg@NTSCConfig {..} pixels = do
  let TimingParams {..} = cfgTiming
      SignalParams {..} = cfgSignal

  -- generate line segments in order
  let frontPorch = generateBlankingLevel cfgSignal tpFrontPorch tpSampleRate
  let syncPulse = generateSyncPulse cfgSignal HorizontalSync tpSyncPulseDuration tpSampleRate
  let breezeway = generateBlankingLevel cfgSignal tpBreezeway tpSampleRate

  -- color burst (saveand restore phase for burst)
  savedPhase <- get
  let burstPhase = Phase 0 -- burst phase is fixed relative to subcarrier
      burst = ColorBurst tpColorSubcarrier burstPhase spBurstLevel tpColorBurstDuration
  put $ initPhaseAccumulator tpColorSubcarrier tpSampleRate burstPhase
  let colorBurst = generateColorBurst burst tpSampleRate
  put savedPhase -- restore phase for active video
  let backPorch = generateBlankingLevel cfgSignal tpBackPorch tpSampleRate
  activeVideo <- generateActiveVideo cfg pixels tpActiveLineDuration

  -- concatenate all segments
  return $ VU.concat [frontPorch, syncPulse, breezeway, colorBurst, backPorch, activeVideo]

-- | Modulate chrominance components onto subcarrier
-- modulateChroma :: Frequency -> YIQ -> State PhaseAccumulator (Vector Sample)
-- modulateChroma sampeRate yiq@YIQ {..} = do
modulateChroma :: State PhaseAccumulator (Vector Sample)
modulateChroma = do
  -- TODO: for now, return empty vector as its handled in generatePixelSamples
  -- this function is here for API compatibility and future optimization
  return VU.empty

-- | Combine luminance and chrominance signals
combineSignals :: LuminanceSignal -> ChrominanceSignal -> CompositeSignal
combineSignals (LuminanceSignal lumaSamples) (ChrominanceSignal chromaI chromaQ) = CompositeSignal $ zipWith3 combine lumaSamples chromaI chromaQ
  where
    combine (Sample y) (Sample i) (Sample q) = Sample (y + i + q)

-- | Generate sine wave samples
generateSineWave :: Frequency -> Phase -> Time -> Frequency -> Vector Double
generateSineWave freq phase duration sampleRate =
  VU.generate numSamples $ \i ->
    let t = fromIntegral i / unFrequency sampleRate
     in sin (2 * pi * unFrequency freq * t + unPhase phase)
  where
    numSamples = secondsToSamples sampleRate duration

-- | Generate cosine wave samples
generateCosineWave :: Frequency -> Phase -> Time -> Frequency -> Vector Double
generateCosineWave freq phase duration sampleRate =
  VU.generate numSamples $ \i ->
    let t = fromIntegral i / unFrequency sampleRate
     in cos (2 * pi * unFrequency freq * t + unPhase phase)
  where
    numSamples = secondsToSamples sampleRate duration

-- | Resample signal to different sample rate
resampleSignal :: Frequency -> Frequency -> Vector Sample -> Vector Sample
resampleSignal oldRate newRate samples
  | oldRate == newRate = samples
  | otherwise = VU.generate newLength $ \i ->
      let srcIdx = fromIntegral i * unFrequency oldRate / unFrequency newRate
          idx0 = floor srcIdx :: Int
          idx1 = min (idx0 + 1) (VU.length samples - 1)
          frac = srcIdx - fromIntegral idx0
       in if idx0 >= VU.length samples
            then Sample 0
            else
              let Sample s0 = samples VU.! idx0
                  Sample s1 = samples VU.! idx1
               in Sample (s0 * (1 - frac) + s1 * frac)
  where
    newLength = round $ fromIntegral (VU.length samples) * unFrequency newRate / unFrequency oldRate

-- Helper function for zipWith3 on lists
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 f (a : as) (b : bs) (c : cs) = f a b c : zipWith3 f as bs cs
zipWith3 _ _ _ _ = []