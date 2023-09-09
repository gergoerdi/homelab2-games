module Snake where

import Z80
import Data.Word

videoStart :: Word16
videoStart = 0xc001

numCols :: (Num a) => a
numCols = 40

numRows :: (Num a) => a
numRows = 22

space :: Word8
space = 0x20

data Locations = MkLocs
  { headIdx :: Location
  , tailIdx :: Location
  , growth :: Location
  , segmentLo, segmentHi, segmentChar :: Location
  , newHead :: Location
  , bodyDispatchTrampoline :: Location
  , bodyDispatch :: Location
  , slitherF, lfsr10F, isInBoundsF, randomizeF, placeFruitF :: Location
  , lastInput, currentDir :: Location
  , rng :: Location
  , fruitLoc, fruitNum :: Location
  , score, speed, lives :: Location
  }
