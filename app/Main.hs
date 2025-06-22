module Main (main) where

import CLI (Command (..), execParser, opts, runDecode, runEncode, runInfo)

-- import System.Exit (exitFailure)
-- import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Encode encOpts -> runEncode encOpts
    Decode decOpts -> runDecode decOpts
    Info infoOpts -> runInfo infoOpts
