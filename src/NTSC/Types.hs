{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module NTSC.Types
  ( -- * Core Types
    Sample (..),
    IRELevel (..),
    Time (..),
    Frequency (..),
    Phase (..),

    -- * Color Types
    RGB (..),
    YIQ (..),
    YUV (..),
    ColorSpace (..),

    -- * Signal Components
    LuminanceSignal (..),
    ChrominanceSignal (..),
    CompositeSignal (..),
    SyncPulse (..),
    SyncType (..),
    ColorBurst (..),

    -- * Frame Structure
    Field (..),
    FieldType (..),
    Frame (..),
    Line (..),
    LineType (..),
    Pixel (..),

    -- * Timing Parameters
    TimingParams (..),
    defaultNTSCTiming,

    -- * Signal Parameters
    SignalParams (..),
    defaultNTSCSignal,

    -- * NTSC Configuration
    NTSCStandard (..),
    NTSCConfig (..),
    defaultNTSCConfig,

    -- * Utility Functions
    ireToVoltage,
    voltageToIRE,
    secondsToSamples,
    samplesToSeconds,
  )
where

import Data.Vector.Generic (Vector (basicLength))
import qualified Data.Vector.Generic as VG
import Data.Vector.Generic.Mutable (MVector (basicInitialize))
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)

-- | A single sample value (voltage or normalized amplitude)
newtype Sample = Sample {unSample :: Double}
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Generic)

newtype instance VU.MVector s Sample = MV_Sample (VU.MVector s Double)

newtype instance VU.Vector Sample = V_Sample (VU.Vector Double)

instance VGM.MVector VU.MVector Sample where
  {-# INLINE basicLength #-}
  basicLength (MV_Sample v) = VGM.basicLength v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (MV_Sample v) = MV_Sample $ VGM.basicUnsafeSlice i n v
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MV_Sample v1) (MV_Sample v2) = VGM.basicOverlaps v1 v2
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n = MV_Sample <$> VGM.basicUnsafeNew n
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_Sample v) i = Sample <$> VGM.basicUnsafeRead v i
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_Sample v) i (Sample x) = VGM.basicUnsafeWrite v i x

  {-# INLINE basicInitialize #-}
  basicInitialize (MV_Sample v) = VGM.basicInitialize v

instance VG.Vector VU.Vector Sample where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_Sample v) = V_Sample <$> VG.basicUnsafeFreeze v
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_Sample v) = MV_Sample <$> VG.basicUnsafeThaw v
  {-# INLINE basicLength #-}
  basicLength (V_Sample v) = VG.basicLength v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (V_Sample v) = V_Sample $ VG.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_Sample v) i = Sample <$> VG.basicUnsafeIndexM v i

instance VU.Unbox Sample

-- | IRE (Institute of Radio Engineers) units for NTSC signal levels
newtype IRELevel = IRELevel {unIRE :: Double}
  deriving (Show, Eq, Ord, Num, Fractional, Generic)

-- | Time in seconds
newtype Time = Time {unTime :: Double}
  deriving (Show, Eq, Ord, Num, Fractional, Generic)

-- | Frequency in Hz
newtype Frequency = Frequency {unFrequency :: Double}
  deriving (Show, Eq, Ord, Num, Fractional, Generic)

-- | Phase in radians
newtype Phase = Phase {unPhase :: Double}
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Generic)

-- | RGB color representation (gamma-corrected values 0-1)
data RGB = RGB
  { rgbR :: !Double,
    rgbG :: !Double,
    rgbB :: !Double
  }
  deriving (Show, Eq, Generic)

-- | YIQ color representation
data YIQ = YIQ
  { -- | Luminance (0-1)
    yiqY :: !Double,
    -- | In-phase chrominance
    yiqI :: !Double,
    -- | Quadrature chrominance
    yiqQ :: !Double
  }
  deriving (Show, Eq, Generic)

-- | YUV color representation
data YUV = YUV
  { -- | Luminance
    yuvY :: !Double,
    -- | Blue-difference
    yuvU :: !Double,
    -- | Red-difference
    yuvV :: !Double
  }
  deriving (Show, Eq, Generic)

-- | Supported color spaces
data ColorSpace = CSRgb | CSYiq | CSYuv
  deriving (Show, Eq, Generic)

-- | Luminance (brightness) signal
newtype LuminanceSignal = LuminanceSignal [Sample]
  deriving (Show, Eq, Generic)

-- | Chrominance (color) signal
data ChrominanceSignal = ChrominanceSignal
  { -- | In-phase component
    chromaI :: ![Sample],
    -- | Quadrature component
    chromaQ :: ![Sample]
  }
  deriving (Show, Eq, Generic)

-- | Composite NTSC signal (luminance + chrominance + sync)
newtype CompositeSignal = CompositeSignal [Sample]
  deriving (Show, Eq, Generic)

-- | Type of sync pulse
data SyncType = HorizontalSync | VerticalSync | EqualizingPulse
  deriving (Show, Eq, Generic)

-- | Sync pulse information
data SyncPulse = SyncPulse
  { syncType :: !SyncType,
    syncDuration :: !Time,
    syncLevel :: !IRELevel
  }
  deriving (Show, Eq, Generic)

-- | Color burst reference signal
data ColorBurst = ColorBurst
  { burstFrequency :: !Frequency,
    burstPhase :: !Phase,
    burstAmplitude :: !IRELevel,
    burstDuration :: !Time
  }
  deriving (Show, Eq, Generic)

-- | Pixel data
data Pixel = Pixel
  { pixelColor :: !RGB,
    -- | (x, y) position
    pixelPosition :: !(Int, Int)
  }
  deriving (Show, Eq, Generic)

-- | Type of video line
data LineType
  = -- | Contains picture information
    ActiveVideo
  | -- | Horizontal blanking interval
    HorizontalBlank
  | -- | Vertical blanking interval
    VerticalBlank
  deriving (Show, Eq, Generic)

-- | A single scan line
data Line = Line
  { lineType :: !LineType,
    lineNumber :: !Int,
    linePixels :: ![Pixel],
    -- | Composite signal samples
    lineSamples :: ![Sample]
  }
  deriving (Show, Eq, Generic)

-- | Field type for interlaced video
data FieldType = OddField | EvenField
  deriving (Show, Eq, Generic)

-- | A single field (half-frame in interlaced video)
data Field = Field
  { fieldType :: !FieldType,
    fieldLines :: ![Line]
  }
  deriving (Show, Eq, Generic)

-- | A complete frame (two fields interlaced NTSC)
data Frame = Frame
  { frameNumber :: !Int,
    frameOdd :: !Field,
    frameEven :: !Field
  }
  deriving (Show, Eq, Generic)

-- | NTSC timing parameters
data TimingParams = TimingParams
  { -- | Digital sampling rate
    tpSampleRate :: !Frequency,
    -- | Horizontal scan rate (~15.734 kHz)
    tpLineFrequency :: !Frequency,
    -- | Field rate (~59.94 Hz)
    tpFieldFrequency :: !Frequency,
    -- | Color subcarrier (~3.579545 MHz)
    tpColorSubcarrier :: !Frequency,
    -- | Total line time (~63.556 μs)
    tpLineDuration :: !Time,
    -- | Active video portion (~52.85 μs)
    tpActiveLineDuration :: !Time,
    -- | Horizontal blanking (~10.7 μs)
    tpHBlankDuration :: !Time,
    -- | Front porch duration (~1.5 μs)
    tpFrontPorch :: !Time,
    -- | H-sync pulse (~4.7 μs)
    tpSyncPulseDuration :: !Time,
    -- | Between sync and burst (~0.6 μs)
    tpBreezeway :: !Time,
    -- | Color burst (~2.5 μs)
    tpColorBurstDuration :: !Time,
    -- | Back porch duration (~1.6 μs)
    tpBackPorch :: !Time,
    -- | 262.5 lines per field
    tpLinesPerField :: !Int,
    -- | 525 lines per frame
    tpLinesPerFrame :: !Int,
    -- | ~480 visible lines
    tpActiveLines :: !Int
  }
  deriving (Show, Eq, Generic)

-- | Default NTSC-M timing parameters
defaultNTSCTiming :: TimingParams
defaultNTSCTiming =
  TimingParams
    { tpSampleRate = Frequency 14318180, -- 4 * fsc
      tpLineFrequency = Frequency 15734.264,
      tpFieldFrequency = Frequency 59.94,
      tpColorSubcarrier = Frequency 3579545,
      tpLineDuration = Time 63.556e-6,
      tpActiveLineDuration = Time 52.85e-6,
      tpHBlankDuration = Time 10.7e-6,
      tpFrontPorch = Time 1.5e-6,
      tpSyncPulseDuration = Time 4.7e-6,
      tpBreezeway = Time 0.6e-6,
      tpColorBurstDuration = Time 2.5e-6,
      tpBackPorch = Time 1.6e-6,
      tpLinesPerField = 262, -- Actually 262.5
      tpLinesPerFrame = 525,
      tpActiveLines = 480
    }

-- | NTSC signal level parameters
data SignalParams = SignalParams
  { -- | Sync tip level (-40 IRE)
    spSyncLevel :: !IRELevel,
    -- | Blanking level (0 IRE)
    spBlankingLevel :: !IRELevel,
    -- | Black level (7.5 IRE in NA)
    spBlackLevel :: !IRELevel,
    -- | White level (100 IRE)
    spWhiteLevel :: !IRELevel,
    -- | Color burst amplitude (±20 IRE)
    spBurstLevel :: !IRELevel,
    -- | Black setup/pedestal (7.5 IRE)
    spSetup :: !IRELevel
  }
  deriving (Show, Eq, Generic)

-- | Default NTSC-M signal parameters (North America)
defaultNTSCSignal :: SignalParams
defaultNTSCSignal =
  SignalParams
    { spSyncLevel = IRELevel (-40),
      spBlankingLevel = IRELevel 0,
      spBlackLevel = IRELevel 7.5,
      spWhiteLevel = IRELevel 100,
      spBurstLevel = IRELevel 20,
      spSetup = IRELevel 7.5
    }

-- | NTSC standard variations
data NTSCStandard
  = -- | North America (525/60, 7.5 IRE setup)
    NTSC_M
  | -- | Japan (525/60, 0 IRE setup)
    NTSC_J
  | -- | NTSC with PAL subcarrier
    NTSC_443
  deriving (Show, Eq, Generic, Read)

-- | Complete NTSC configuration
data NTSCConfig = NTSCConfig
  { cfgStandard :: !NTSCStandard,
    cfgTiming :: !TimingParams,
    cfgSignal :: !SignalParams,
    -- | Gamma correction factor (2.2)
    cfgGamma :: !Double
  }
  deriving (Show, Eq, Generic)

-- | Default NTSC configuration (NTSC-M)
defaultNTSCConfig :: NTSCConfig
defaultNTSCConfig =
  NTSCConfig
    { cfgStandard = NTSC_M,
      cfgTiming = defaultNTSCTiming,
      cfgSignal = defaultNTSCSignal,
      cfgGamma = 2.2
    }

-- | Convert IRE units to voltage (assuming 1V peak-to-peak)
ireToVoltage :: IRELevel -> Double
ireToVoltage (IRELevel ire) = 0.714 * (ire / 140.0) + 0.286

-- | Convert voltage to IRE units
voltageToIRE :: Double -> IRELevel
voltageToIRE v = IRELevel $ 140.0 * (v - 0.286) / 0.714

-- | Convert time duration to number of samples
secondsToSamples :: Frequency -> Time -> Int
secondsToSamples (Frequency fs) (Time t) = round (fs * t)

-- | Convert number of samples to time duration
samplesToSeconds :: Frequency -> Int -> Time
samplesToSeconds (Frequency fs) n = Time (fromIntegral n / fs)