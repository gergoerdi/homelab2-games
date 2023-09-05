{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Snake.Main (snake) where

import Z80
import Data.Word
import Control.Monad
import Data.Bits
import Data.Char

data Locations = MkLocs
  { headIdx :: Location
  , tailIdx :: Location
  , segmentLo, segmentHi, segmentChar :: Location
  }

videoStart :: Word16
videoStart = 0xc001

numCols :: (Num a) => a
numCols = 40

numRows :: (Num a) => a
numRows = 25

space :: Word8
space = 0x20
-- space = 0xfb

wall :: Word8
wall = 0xfb
-- wall = 0xa0

snake :: Z80ASM
snake = do
    clearScreen
    drawBorder
    rec
        drawSnake locs
        tailIdx <- labelled $ db [0]
        headIdx <- labelled $ db [5]
        segmentLo <- labelled $ db $ take 256 $ [100, 60, 61, 62, 63, 64, 110, 120] ++ repeat 0
        segmentHi <- labelled $ db $ take 256 $ repeat 0
        segmentChar <- labelled $ db $ take 256 $ [bodyNS, bodySE, bodyEW, bodyEW, bodyEW, headE] ++ repeat 0
        let locs = MkLocs{..}
    pure ()

clearScreen :: Z80ASM
clearScreen = do
    ld HL videoStart
    rec loop <- label
        ld [HL] space
        inc HL
        ld A H
        cp 0xc4
        jr NZ loop
    pure ()

drawBorder :: Z80ASM
drawBorder = do
    ld HL videoStart
    decLoopB numCols do
        ld [HL] wall
        inc HL
    ld DE (numCols - 1)
    decLoopB (numRows - 2) do
        ld [HL] wall
        add HL DE
        ld [HL] wall
        inc HL
    decLoopB numCols do
        ld [HL] wall
        inc HL

bodyEW, bodyNS, bodySE :: Word8
bodyEW = 0x91
bodyNS = 0x90
bodySE = 0x6e

headE :: Word8
headE = 0x8d

drawSnake :: Locations -> Z80ASM
drawSnake MkLocs{..} = do
    ldVia A C [tailIdx]
    ldVia A B [headIdx]

    withLabel \loop -> do
        -- Copy (0, C) to BC'
        push BC
        exx
        pop BC
        ld B 0
        exx

        -- Set HL to target video address
        loadArrayBC' D segmentHi
        loadArrayBC' E segmentLo
        ld HL videoStart
        add HL DE

        -- Draw segment
        loadArrayBC' A segmentChar
        ld [HL] A

        ld A C
        cp B
        inc C
        jp NZ loop

-- | Pre: `BC'` contains the index
-- | Post: `target` contains the value at `(base + index)`
loadArrayBC' :: (Load r [RegIx]) => r -> Location -> Z80ASM
loadArrayBC' target base = do
    ld IX base
    exx
    add IX BC
    exx
    ld target [IX]
