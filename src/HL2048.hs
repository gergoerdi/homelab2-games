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

tileHeight :: Num a => a
tileHeight = 2

tileWidth :: Num a => a
tileWidth = 4

data Locations = MkLocs
  { drawTileF :: Location
  , calcMoveF :: Location
  , screenBuf :: Location
  , tileValues, tileSpeeds, tileOffs :: Location
  }
