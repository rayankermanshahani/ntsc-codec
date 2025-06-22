{-# LANGUAGE RecordWildCards #-}

module NTSC.Color
  ( -- * Color Conversions
    rgbToYiq,
    yiqToRgb,
    rgbToYuv,
    yuvToRgb,
    yiqToYuv,
    yuvToYiq,

    -- * Gamma Correction
    gammaCorrect,
    gammaUncorrect,

    -- * Color Difference Signals
    ColorDifference (..),
    rgbToColorDiff,
    colorDiffToRgb,

    -- * NTSC Color Matrix Operations
    ntscEncoderMatrix,
    ntscDecoderMatrix,
    applyColorMatrix,
  )
where

import Linear.Matrix (M33, (!*))
import Linear.V3 (V3 (..))
import NTSC.Types

-- | Color difference signals used in NTSC
data ColorDifference = ColorDifference
  { -- | Luminance (Y)
    cdY :: !Double,
    -- | R - Y
    cdRY :: !Double,
    -- | B - Y
    cdBY :: !Double
  }
  deriving (Show, Eq)

-- | Convert RGB to YIQ color space using NTSC 1953 color primaries
rgbToYiq :: RGB -> YIQ
rgbToYiq RGB {..} =
  YIQ
    { yiqY = 0.299 * rgbR + 0.587 * rgbG + 0.114 * rgbB,
      yiqI = 0.596 * rgbR - 0.274 * rgbG - 0.322 * rgbB,
      yiqQ = 0.211 * rgbR - 0.523 * rgbG + 0.312 * rgbB
    }

-- | Convert YIQ to RGB color space
yiqToRgb :: YIQ -> RGB
yiqToRgb YIQ {..} =
  RGB
    { rgbR = clamp $ yiqY + 0.956 * yiqI + 0.621 * yiqQ,
      rgbG = clamp $ yiqY - 0.272 * yiqI - 0.647 * yiqQ,
      rgbB = clamp $ yiqY - 1.106 * yiqI + 1.703 * yiqQ
    }
  where
    clamp x = max 0 (min 1 x)

-- | Convert RGB to YUV color space
-- U and V are scaled versions of B-Y and R-Y
rgbToYuv :: RGB -> YUV
rgbToYuv RGB {..} =
  YUV
    { yuvY = y,
      yuvU = 0.493 * (rgbB - y), -- Scaled B-Y
      yuvV = 0.877 * (rgbR - y) -- Scaled R-Y
    }
  where
    y = 0.299 * rgbR + 0.587 * rgbG + 0.114 * rgbB

-- | Convert YUV to RGB color space
yuvToRgb :: YUV -> RGB
yuvToRgb YUV {..} =
  RGB
    { rgbR = clamp $ yuvY + 1.140 * yuvV,
      rgbG = clamp $ yuvY - 0.394 * yuvU - 0.581 * yuvV,
      rgbB = clamp $ yuvY + 2.032 * yuvU
    }
  where
    clamp x = max 0 (min 1 x)

-- | Convert YIQ to YUV
-- This is a rotation by ~33 degrees
yiqToYuv :: YIQ -> YUV
yiqToYuv YIQ {..} =
  YUV
    { yuvY = yiqY, -- Luminance is the same
      yuvU = (-0.544639) * yiqI + 0.838670 * yiqQ,
      yuvV = 0.838670 * yiqI + 0.544639 * yiqQ
    }

-- Convert YUV to YIQ
yuvToYiq :: YUV -> YIQ
yuvToYiq YUV {..} =
  YIQ
    { yiqY = yuvY, -- Luminance is the same
      yiqI = (-0.544639) * yuvU + 0.838670 * yuvV,
      yiqQ = 0.838670 * yuvU + 0.544639 * yuvV
    }

-- | Convert RGB to color difference signals
rgbToColorDiff :: RGB -> ColorDifference
rgbToColorDiff RGB {..} =
  ColorDifference
    { cdY = y,
      cdRY = rgbR - y,
      cdBY = rgbB - y
    }
  where
    y = 0.299 * rgbR + 0.587 * rgbG + 0.114 * rgbB

-- | Convert color difference signals to RGB
colorDiffToRgb :: ColorDifference -> RGB
colorDiffToRgb ColorDifference {..} =
  RGB
    { rgbR = clamp $ cdY + cdRY,
      rgbG = clamp $ cdY - 0.509 * cdRY - 0.194 * cdBY,
      rgbB = clamp $ cdY + cdBY
    }
  where
    clamp x = max 0 (min 1 x)

-- | Apply gamma correction to linear RGB values
gammaCorrect :: Double -> RGB -> RGB
gammaCorrect gamma RGB {..} =
  RGB
    { rgbR = rgbR ** (1.0 / gamma),
      rgbG = rgbG ** (1.0 / gamma),
      rgbB = rgbB ** (1.0 / gamma)
    }

-- | Remove gamma correction from linear RGB values
gammaUncorrect :: Double -> RGB -> RGB
gammaUncorrect gamma RGB {..} =
  RGB
    { rgbR = rgbR ** gamma,
      rgbG = rgbG ** gamma,
      rgbB = rgbB ** gamma
    }

-- | NTSC encoder matrix (RGB to YIQ)
-- This is the standard NTSC encoding matrix
ntscEncoderMatrix :: [[Double]]
ntscEncoderMatrix =
  [ [0.299, 0.587, 0.114], -- Y
    [0.596, -0.274, -0.322], -- I
    [0.211, -0.523, 0.312] -- Q
  ]

-- | NTSC decoder matrix (YIQ to RGB)
-- This is an inverse of the encoder matrix
ntscDecoderMatrix :: [[Double]]
ntscDecoderMatrix =
  [ [1.000, 0.956, 0.621], -- R
    [1.000, -0.272, -0.647], -- G
    [1.000, -1.106, 1.703] -- B
  ]

-- | Apply a 3x3 color matrix to transform between color spaces
-- applyColorMatrix :: [[Double]] -> (Double, Double, Double) -> (Double, Double, Double)
-- applyColorMatrix matrix (x, y, z) =
--   case matrix of
--     [row1, row2, row3] ->
--       ( sum (zipWith (*) row1 [x, y, z]),
--         sum (zipWith (*) row2 [x, y, z]),
--         sum (zipWith (*) row3 [x, y, z])
--       )
--     _ -> error "applyColorMatrix: matrix must be 3x3"
applyColorMatrix :: M33 Double -> V3 Double -> V3 Double
applyColorMatrix m v = m !* v