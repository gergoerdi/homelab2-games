module HL2048 where

import Z80
import Data.Word

videoStart :: Word16
videoStart = 0xc001

videoBufStart :: Word16
videoBufStart = 0x7001

numCols :: (Num a) => a
numCols = 40

numRows :: (Num a) => a
numRows = 25

space :: Word8
space = 0x20

data Locations = MkLocs
  { drawTileF :: Location
  , clearTileF :: Location
  , screenBuf :: Location
  , anim :: Location
  , animSub :: Location
  }
