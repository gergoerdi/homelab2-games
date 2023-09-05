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
    rec
        let locs = MkLocs{..}
        clearScreen
        drawBorder
        drawSnake locs
        loopForever $ pure ()

        tailIdx <- labelled $ db [0]
        headIdx <- labelled $ db [6]
        segmentLo <- labelled $ db $ take 256 $ [100, 60, 61, 62, 63, 64, 110, 120] ++ repeat 0
        segmentHi <- labelled $ db $ take 256 $ repeat 0
        segmentChar <- labelled $ db $ take 256 $ [bodyNS, bodySE, bodyEW, bodyEW, bodyEW, headE] ++ repeat 0
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
    ld B 0
    ldVia A C [tailIdx]
    ld A [headIdx]

    withLabel \loop -> do
        -- Set HL to target video address
        loadArray E segmentLo BC
        loadArray D segmentHi BC
        ld HL videoStart
        add HL DE

        -- Draw segment
        loadArray D segmentChar BC
        ld [HL] D

        -- Compare iterator C with head A
        inc C
        cp C
        jp NZ loop

-- | Pre: `BC` contains the index
-- | Post: `target` contains the value at `(base + index)`
loadArray :: (Load r [RegIx], Arithmetic RegIx r') => r -> Location -> r' -> Z80ASM
loadArray target base idx = do
    ld IX base
    add IX idx
    ld target [IX]
