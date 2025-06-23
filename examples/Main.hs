module Main (main) where

import Control.Monad.State.Strict
import qualified Data.Vector.Unboxed as VU
import NTSC.Signal
import NTSC.Types
import Text.Printf

main :: IO ()
main = do
  putStrLn "NTSC Signal Generation Example"
  putStrLn "=============================="

  let cfg = defaultNTSCConfig
      colorBars = generateColorBars 8 -- 8 standard color bars

      -- Initialize phase accumulator for color subcarrier
      pa =
        initPhaseAccumulator
          (tpColorSubcarrier $ cfgTiming cfg)
          (tpSampleRate $ cfgTiming cfg)
          (Phase 0)

      -- Generate one complete line
      (lineSignal, _) = runState (assembleHorizontalLine cfg colorBars) pa

  -- Print signal statistics
  printSignalStats lineSignal

  -- Save a few samples for inspection
  putStrLn "\nFirst 100 samples (time, voltage):"
  printSamples (tpSampleRate $ cfgTiming cfg) (VU.take 100 lineSignal)

-- | Generate standard SMPTE color bars
generateColorBars :: Int -> [Pixel]
generateColorBars numBars =
  concatMap makeBar [0 .. numBars - 1]
  where
    pixelsPerBar = 80 -- 640 pixels / 8 bars

    -- Standard color bar colors (white, yellow, cyan, green, magenta, red, blue, black)
    barColors =
      [ RGB 1.0 1.0 1.0, -- White
        RGB 1.0 1.0 0.0, -- Yellow
        RGB 0.0 1.0 1.0, -- Cyan
        RGB 0.0 1.0 0.0, -- Green
        RGB 1.0 0.0 1.0, -- Magenta
        RGB 1.0 0.0 0.0, -- Red
        RGB 0.0 0.0 1.0, -- Blue
        RGB 0.0 0.0 0.0 -- Black
      ]

    makeBar barIdx =
      let color = barColors !! (barIdx `mod` length barColors)
       in replicate pixelsPerBar (Pixel color (barIdx * pixelsPerBar, 0))

-- | Print signal statistics
printSignalStats :: VU.Vector Sample -> IO ()
printSignalStats samples = do
  let sampleList = map unSample (VU.toList samples)
      minVal = minimum sampleList
      maxVal = maximum sampleList
      avgVal = sum sampleList / fromIntegral (length sampleList)

      -- Convert to IRE units for display
      minIRE = unIRE $ voltageToIRE minVal
      maxIRE = unIRE $ voltageToIRE maxVal
      avgIRE = unIRE $ voltageToIRE avgVal

  putStrLn $ printf "\nSignal Statistics:"
  putStrLn $ printf "  Samples: %d" (VU.length samples)
  putStrLn $ printf "  Min: %.3f V (%.1f IRE)" minVal minIRE
  putStrLn $ printf "  Max: %.3f V (%.1f IRE)" maxVal maxIRE
  putStrLn $ printf "  Avg: %.3f V (%.1f IRE)" avgVal avgIRE

-- | Print sample values with timestamps
printSamples :: Frequency -> VU.Vector Sample -> IO ()
printSamples (Frequency sampleRate) samples = do
  VU.iforM_ samples $ \i (Sample v) -> do
    let t = fromIntegral i / sampleRate * 1e6 -- Convert to microseconds
        ire = unIRE $ voltageToIRE v
    putStrLn $ printf "%6.2f µs: %7.4f V (%6.1f IRE)" t v ire