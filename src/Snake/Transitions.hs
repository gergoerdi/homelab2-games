{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Snake.Transitions where

import Snake

import Z80
import Z80.Utils
import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char

transition :: Word8 -> (Locations -> Z80ASM) -> Locations -> Z80ASM
transition filler effect locs@MkLocs{..} = skippable \end -> mdo
    ld A filler
    call fill
    decLoopB 10 halt
    ld A space
    call fill
    jp end

    fill <- labelled do
        effect locs
        ret
    pure ()
  where

curtainH :: Locations -> Z80ASM
curtainH _ = do
    ld HL $ videoStart + numCols + 1
    ld D 0
    ld E $ numCols - 3
    ld C 1
    decLoopB (numCols `div` 2 - 1) do
        exx
        decLoopB (numRows - 2) do
            exx
            ld [HL] A
            add HL DE
            ld [HL] A
            push DE
            ld E C
            inc HL
            add HL DE
            add HL DE
            pop DE
            exx
        exx
        halt
        ld HL $ videoStart + numCols + 1
        push DE
        ld E C
        add HL DE
        pop DE
        dec E
        dec E
        inc C

curtainV :: Locations -> Z80ASM
curtainV _ = do
    ld HL (videoStart + numCols + 1)
    decLoopB (numRows - 2) do
        push HL
        exx
        pop HL
        decLoopB (numCols - 2) do
            ld [HL] A
            inc HL
        inc HL
        inc HL
        push HL
        exx
        pop HL
        replicateM_ 1 halt

scramble :: Locations -> Z80ASM
scramble MkLocs{..} = do
    ld DE 1
    exx
    ld C A
    exx
    decLoopB 4 do
        push DE
        exx
        pop DE
        decLoopB 0x100 $ skippable \skip -> do
            call lfsr10F

            ld A D
            Z80.and 0x03
            Z80.or 0xc0
            ld H A
            ld L E

            call isInBoundsF
            jp Z skip
            ldVia A [HL] C
        push DE
        exx
        pop DE
