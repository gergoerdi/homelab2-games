module Snake where

import Z80
import Data.Word

videoBufStart :: Word16
videoBufStart = 0x7001

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
  , moveF, slitherF, lfsr10F, isInBoundsF, randomizeF, placeFruitF
  , latchInputF, clearBufF, waitInputF
  , lastInput, currentDir
  , drawScoreF, gameOverTransitionF
  , attractF
  , rng
  , fruitLoc, fruitNum
  , score, speed, lives :: Location
  }
