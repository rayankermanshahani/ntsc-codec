module Main (main) where

import CLI (Command (..), execParser, opts, runDecode, runEncode, runInfo)

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Encode encOpts -> runEncode encOpts
    Decode decOpts -> runDecode decOpts
    Info infoOpts -> runInfo infoOpts