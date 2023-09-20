module Snake where

import Z80
import Data.Word

videoStart :: Word16
videoStart = 0xc001

videoBufStart :: Word16
videoBufStart = 0x7001

numCols :: (Num a) => a
numCols = 40

numRows :: (Num a) => a
numRows = 22

space :: Word8
space = 0x20

data Locations = MkLocs
  { headIdx, tailIdx, growth
  , segmentLo, segmentHi, segmentChar
  , newHead
  , bodyDispatchTrampoline
  , bodyDispatch
  , slitherF, lfsr10F, isInBoundsF, randomizeF, placeFruitF
  , latchInputF, clearBufF, waitInputF
  , lastInput, currentDir
  , drawScoreF
  , rng
  , fruitLoc, fruitNum
  , score, speed, lives :: Location
  }
