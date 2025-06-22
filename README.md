# ntsc-codec

An NTSC modulator / demodulator written in pure Haskell as a command-line tool.

### Installation Instructions
1. Clone this repo: 
```bash
$ git clone https://github.com/rayankermanshahani/ntsc-codec && cd ntsc-codec
```  
2. Build the executable: 
```bash
$ cabal build
```

### Usage Instructions
Getting help: 
```bash
$ cabal run ntsc-codec -- -h

ntsc-codec - A software NTSC vidoe codec implementation

Usage: ntsc-codec COMMAND

  NTSC video encoder/decoder

Available options:
  -h,--help                Show this help text

Available commands:
  encode                   Encode video to NTSC signal
  decode                   Decode vidoe from NTSC signal
  info                     Show information about NTSC file
```
