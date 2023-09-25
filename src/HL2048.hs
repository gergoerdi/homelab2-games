module HL2048 where

import Z80
import Data.Word

videoBufStart :: Word16
videoBufStart = 0x7001

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
  , calcAnimF :: Location
  , screenBuf :: Location
  , tileValues, tileValues', tileSpeeds, tileOffs :: Location
  , rng :: Location
  }
