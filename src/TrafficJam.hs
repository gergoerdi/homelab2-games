module TrafficJam where

import Z80
import Data.Word

videoBufStart :: Word16
videoBufStart = 0x7001

numRows :: (Num a) => a
numRows = 25

space :: Word8
space = 0x20

data Locations = MkLocs
  { player, positions, sizes, orients :: Location
  }
