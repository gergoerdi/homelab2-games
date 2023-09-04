{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Tetris.Main (tetris) where

import Tetris
import Tetris.Draw

import Z80
import Data.Word
import Data.Bits

tetris :: Z80ASM
tetris = do
    clearScreen
    rec
        drawSkeleton locs
        drawTetris locs
        loopForever $ pure ()
        pieces <- labelled $ dw allPieces
        state <- labelled $ dw testState
        let locs = MkLocs{..}

    pure ()

testState :: [Word16]
testState = map (`shiftL` 4) $
  [ 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00011_00000_1
  , 0b1_00001_10000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00011_11000_1
  , 0b1_01001_11100_1
  , 0b1_11111_11100_1
  , 0b1_11111_11110_1
  , 0b1_11111_11111_1
  ]
