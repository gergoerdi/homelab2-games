{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Snake.Transitions where

import Snake

import Z80
import Z80.Utils
import HL2
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
    clearA
    call fill
    jp end

    fill <- labelled do
        effect locs
        ret
    pure ()

mirrorHLtoIX :: Z80ASM
mirrorHLtoIX = mdo
    ldVia A [hack + 2] L
    ld A H
    Z80.and (complement 0xc0)
    Z80.or 0x70
    ld [hack + 3] A
    hack <- labelled $ ld IX 0x0000
    pure ()

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
            skippable \next -> mdo
                cp 0
                jp Z copy

                -- Fill with constant byte
                ld [HL] A
                add HL DE
                ld [HL] A
                jp next

                -- Copy from videoBufStart
                copy <- label
                mirrorHLtoIX
                ldVia A [HL] [IX]
                add HL DE
                add IX DE
                ldVia A [HL] [IX]
                clearA -- Keep A zero for next iteration's check
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
    ld IX (videoBufStart + numCols + 1)
    ld HL (videoStart + numCols + 1)
    push HL
    exx
    pop HL
    exx
    decLoopB (numRows - 2) do
        exx
        decLoopB (numCols - 2) do
            cp 0
            skippable \next -> mdo
                ld [HL] A
                jp NZ next

                ldVia A [HL] [IX]
                clearA
                inc IX
            inc HL
        replicateM_ 2 $ inc HL
        replicateM_ 2 $ inc IX
        replicateM_ 1 halt
        exx

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
        decLoopB 0x100 $ skippable \next -> do
            call lfsr10F

            ld A D
            Z80.and 0x03
            Z80.or 0xc0
            ld H A
            ld L E

            -- call isInBoundsF
            -- jp Z next
            ldVia A [HL] C
            cp 0
            jp NZ next

            mirrorHLtoIX
            ldVia A [HL] [IX]
            clearA
        push DE
        exx
        pop DE
