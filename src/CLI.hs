{-# LANGUAGE RecordWildCards #-}

module CLI
  ( execParser,
    Command (..),
    opts,
    parseEncodeOptions,
    parseDecodeOptions,
    parseInfoOptions,
    runEncode,
    runDecode,
    runInfo,
  )
where

import NTSC.Types
import Options.Applicative

-- import System.Exit (exitFailure)
-- import System.IO (hPutStrLn, stderr)

-- | Main command type
data Command
  = Encode EncodeOptions
  | Decode DecodeOptions
  | Info InfoOptions
  deriving (Show)

-- | Options for encoding video to NTSC
data EncodeOptions = EncodeOptions
  { -- | Input file (images, raw video, etc.)
    encInput :: !FilePath,
    -- | Output NTSC signal file
    encOutput :: !FilePath,
    -- | Input format
    encFormat :: !InputFormat,
    -- | NTSC standard to use
    encStandard :: !NTSCStandard,
    -- | Override sample rate
    encSampleRate :: !(Maybe Frequency),
    -- | Use interlaced output
    encInterlaced :: !Bool,
    -- | Verbose output
    encVerbose :: !Bool
  }
  deriving (Show)

-- | Options for decoding video from NTSC
data DecodeOptions = DecodeOptions
  { -- | Input NTSC file
    decInput :: !FilePath,
    -- | Output file (images, raw video, etc.)
    decOutput :: !FilePath,
    -- | Output format
    decFormat :: !OutputFormat,
    -- | NTSC standard to use
    decStandard :: !NTSCStandard,
    -- | Override sample rate
    decSampleRate :: !(Maybe Frequency),
    -- | Deinterlace output
    decDeinterlace :: !Bool,
    -- | Use comb filter
    decCombFilter :: !Bool,
    -- | Verbose output
    decVerbose :: !Bool
  }
  deriving (Show)

-- | Options for showing information
data InfoOptions = InfoOptions
  { -- | File to analyze}
    infoInput :: !FilePath,
    -- | Show detailed analysis
    infoDetailed :: !Bool
  }
  deriving (Show)

-- | Supported input formats
data InputFormat
  = -- | PNG image sequence
    InputPNG
  | -- | PPM image sequence
    InputPPM
  | -- | Raw RGB data
    InputRawRGB
  | -- | Raw YUV data
    InputRawYUV
  deriving (Show, Eq, Read)

-- | Supported output formats
data OutputFormat
  = -- | PNG image sequence
    OutputPNG
  | -- | PPM image sequence
    OutputPPM
  | -- | Raw RGB data
    OutputRawRGB
  | -- | Raw YUV data
    OutputRawYUV
  | -- | Raw NTSC composite signal
    OutputRawNTSC
  deriving (Show, Read, Eq)

-- | Parse command-line arguments
parseCommand :: Parser Command
parseCommand =
  subparser
    ( command
        "encode"
        ( info
            (Encode <$> parseEncodeOptions)
            (progDesc "Encode video to NTSC signal")
        )
        <> command
          "decode"
          ( info
              (Decode <$> parseDecodeOptions)
              (progDesc "Decode vidoe from NTSC signal")
          )
        <> command
          "info"
          ( info
              (Info <$> parseInfoOptions)
              (progDesc "Show information about NTSC file")
          )
    )

-- | Parse encode options
parseEncodeOptions :: Parser EncodeOptions
parseEncodeOptions =
  EncodeOptions
    <$> strArgument
      ( metavar "INPUT"
          <> help "Input file or directory"
      )
    <*> strArgument
      ( metavar "OUTPUT"
          <> help "Output file or directory"
      )
    <*> option
      auto
      ( long "format"
          <> short 'f'
          <> metavar "FORMAT"
          <> value InputPPM
          <> help "Input format (PNG, PPM, RawRGB, RawYUV)"
      )
    <*> option
      auto
      ( long "standard"
          <> short 's'
          <> metavar "STANDARD"
          <> value NTSC_M
          <> help "NTSC standard (NTSC_M, NTSC_J, NTSC_443)"
      )
    <*> optional
      ( option
          (Frequency <$> auto)
          ( long "sample-rate"
              <> metavar "RATE"
              <> help "Sample rate in Hz (default: 4*fsc)"
          )
      )
    <*> switch
      ( long "interlaced"
          <> short 'i'
          <> help "Use interlaced encoding"
      )
    <*> switch
      ( long "verbose"
          <> short 'b'
          <> help "Verbose output"
      )

-- | Parse decode options
parseDecodeOptions :: Parser DecodeOptions
parseDecodeOptions =
  DecodeOptions
    <$> strArgument
      ( metavar "INPUT"
          <> help "Input NTSC signal file"
      )
    <*> strArgument
      ( metavar "OUTPUT"
          <> help "Output file or directory"
      )
    <*> option
      auto
      ( long "format"
          <> short 'f'
          <> metavar "FORMAT"
          <> value OutputPPM
          <> help "Input format (PNG, PPM, RawRGB, RawYUV, RawNTSC)"
      )
    <*> option
      auto
      ( long "standard"
          <> short 's'
          <> metavar "STANDARD"
          <> value NTSC_M
          <> help "NTSC standard (NTSC_M, NTSC_J, NTSC_443)"
      )
    <*> optional
      ( option
          (Frequency <$> auto)
          ( long "sample-rate"
              <> metavar "RATE"
              <> help "Sample rate in Hz (default: 4*fsc)"
          )
      )
    <*> switch
      ( long "deinterlace"
          <> short 'd'
          <> help "Deinterlace output"
      )
    <*> switch
      ( long "comb-filter"
          <> short 'c'
          <> help "Use comb filter for better color separation"
      )
    <*> switch
      ( long "verbose"
          <> short 'b'
          <> help "Verbose output"
      )

-- | Parse info options
parseInfoOptions :: Parser InfoOptions
parseInfoOptions =
  InfoOptions
    -- infoInput :: !FilePath,
    -- infoDetailed :: !Bool
    <$> strArgument
      ( metavar "FILE"
          <> help "NTSC file to analyze"
      )
    <*> switch
      ( long "detailed"
          <> short 'd'
          <> help "Show detailed analysis"
      )

-- | Complete parser with version and help
opts :: ParserInfo Command
opts =
  info
    (parseCommand <**> helper)
    ( fullDesc
        <> progDesc "NTSC video encoder/decoder"
        <> header "ntsc-codec - A software NTSC vidoe codec implementation"
    )

-- | Run encoder
runEncode :: EncodeOptions -> IO ()
runEncode EncodeOptions {..} = do
  when encVerbose $ do
    putStrLn $ "Encoding " ++ encInput ++ " to " ++ encOutput
    putStrLn $ "Format: " ++ show encFormat
    putStrLn $ "Standard: " ++ show encStandard
    putStrLn $ "Interlaced: " ++ show encInterlaced

  -- TODO: Implement actual encoding logic
  putStrLn "Encoding logic not yet implemented"
  where
    when True task = task
    when False _ = return ()

-- | Run decoder
runDecode :: DecodeOptions -> IO ()
runDecode DecodeOptions {..} = do
  when decVerbose $ do
    putStrLn $ "Decoding " ++ decInput ++ " to " ++ decOutput
    putStrLn $ "Format: " ++ show decFormat
    putStrLn $ "Standard: " ++ show decStandard
    putStrLn $ "Deinterlace: " ++ show decDeinterlace
    putStrLn $ "Comb filter: " ++ show decCombFilter

  -- TODO: Implement actual decoding logic
  putStrLn "Decoding logic not yet implemented"
  where
    when True task = task
    when False _ = return ()

-- | Run info command
runInfo :: InfoOptions -> IO ()
runInfo InfoOptions {..} = do
  putStrLn $ "Analyzing " ++ infoInput
  when infoDetailed $
    putStrLn "Detailed analysis enabled"

  -- TODO: Imlpement actual info logic
  putStrLn "Info not yet implemented"
  where
    when True task = task
    when False _ = return ()
