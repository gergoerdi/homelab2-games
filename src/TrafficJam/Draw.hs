{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module TrafficJam.Draw where

import TrafficJam
import TrafficJam.Input

import Z80
import Z80.Utils
import HL2

import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char

clearScreen :: Locations -> Z80ASM
clearScreen MkLocs{} = do
    ld HL videoStart
    withLabel \loop -> do
        ld A space
        ld [HL] A
        inc HL
        ld A H
        cp 0xc4
        jp NZ loop
