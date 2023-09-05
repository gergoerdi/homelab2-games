{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Tetris.Draw where

import Tetris

import Z80
import Data.Word
import qualified Data.ByteString as BS
import Control.Monad
import Data.Bits
import Data.Char
import Text.Printf
import Data.Semigroup (stimes)

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
-- wall = 0xfb
wall = 0xa0

well :: Word8
well = 0xfb
-- well = space

clearScreen :: Z80ASM
clearScreen = do
    ld HL videoStart
    loop <- label
    ld [HL] space
    inc HL
    ld A H
    cp 0xc4
    jr NZ loop
    pure ()

unlessFlag :: JumpRelative flag (Location -> Z80ASM) => flag -> Z80ASM -> Z80ASM
unlessFlag flag body = do
    rec jr flag end :: Z80ASM
        body
        end <- label
    pure ()

block :: Word8
block = 0x77

wellStartX = (numCols - (wellWidth + 2)) `div` 2
wellEndX = wellStartX + wellWidth + 1
wellStartY = 3
wellEndY = wellStartY + wellHeight

frameNW, frameN, frameNE, frameW, frameE, frameSW, frameS, frameSE :: Word8
frameNW = 0x6e
frameN  = 0x96
frameNE = 0x6d
frameW  = 0xea
frameE  = 0xeb
frameSW = 0x6c
frameS  = 0x95
frameSE = 0x6b

drawSkeleton :: Locations -> Z80ASM
drawSkeleton MkLocs{..} = do
    -- drawNext
    -- drawLevel
    -- drawPieceStats pieces
    -- drawLineCount
    -- drawScore
    drawWell

drawTetris :: Locations -> Z80ASM
drawTetris locs@MkLocs{..} = do
    ld HL $ videoStart + wellStartY * numCols + wellStartX + 1
    drawLinesFrom wellContents True wellHeight
    drawFallingPiece locs

drawFallingPiece :: Locations -> Z80ASM
drawFallingPiece locs@MkLocs{..} = do
    ld HL $ videoStart + wellStartY * numCols + wellStartX + 1
    ld DE numCols
    ldVia A B [fallHeight]
    withLabel \loop -> do
        add HL DE
        djnz loop
    exx
    -- loadFallingPiece locs
    ld HL pieceBuf
    exx
    drawLines False 4

-- | Post: 'HL' contains address of first line of current falling piece
loadFallingPiece :: Locations -> Z80ASM
loadFallingPiece MkLocs{..} = do
    ld D 0
    ld HL pieces
    ld A [currentPiece]
    replicateM_ 5 rlca
    ld E A
    add HL DE
    ld A [currentRot]
    replicateM_ 3 rlca
    ld E A
    add HL DE


drawWell :: Z80ASM
drawWell = do
    ld HL $ videoStart + wellStartY * numCols + wellStartX
    ld DE $ numCols - (wellWidth + 1)
    ld A well
    exx
    decLoopB wellHeight do
        exx
        ld [HL] wall
        inc HL
        decLoopB wellWidth do
            ld [HL] A
            inc HL
        ld [HL] wall
        add HL DE
        exx

    ld HL $ videoStart + wellEndY * numCols + wellStartX
    ld A wall
    decLoopB (wellWidth + 2) do
        ld [HL] A
        inc HL

drawLineCount :: Z80ASM
drawLineCount = do
    ld HL $ videoStart + 1 * numCols + wellEndX
    ld [HL] frameW
    ld HL $ videoStart + 1 * numCols + wellStartX
    ld [HL] frameE
    inc HL
    forM_ "LINES: 014" \c -> do
        ld [HL] $ fromIntegral $ ord c
        inc HL

    ld HL $ videoStart + 0 * numCols + wellStartX
    ld [HL] frameNW
    inc HL
    replicateM_ wellWidth $ do
        ld [HL] frameN
        inc HL
    ld [HL] frameNE

    ld HL $ videoStart + 2 * numCols + wellStartX
    ld [HL] frameSW
    inc HL
    replicateM_ wellWidth $ do
        ld [HL] frameS
        inc HL
    ld [HL] frameSE

drawScore :: Z80ASM
drawScore = do
    ld HL $ videoStart + 3 * numCols + wellEndX + 3
    ld [HL] frameNW
    inc HL
    replicateM_ 8 do
        ld [HL] frameN
        inc HL
    ld [HL] frameNE

    ld HL $ videoStart + 4 * numCols + wellEndX + 3
    ld [HL] frameW
    inc HL
    forM_ "SCORE:  " \c -> do
        ld [HL] $ fromIntegral $ ord c
        inc HL
    ld [HL] frameE

    ld HL $ videoStart + 5 * numCols + wellEndX + 3
    ld [HL] frameW
    inc HL
    forM_ "00000000" \c -> do
        ld [HL] $ fromIntegral $ ord c
        inc HL
    ld [HL] frameE
    ld HL $ videoStart + 6 * numCols + wellEndX + 3
    ld [HL] frameSW
    inc HL
    replicateM_ 8 do
        ld [HL] frameS
        inc HL
    ld [HL] frameSE

drawLevel :: Z80ASM
drawLevel = do
    ld HL $ videoStart + 21 * numCols + wellEndX + 2
    ld [HL] frameNW
    inc HL
    replicateM_ 9 do
        ld [HL] frameN
        inc HL
    ld [HL] frameNE

    ld HL $ videoStart + 22 * numCols + wellEndX + 2
    ld [HL] frameW
    inc HL
    forM_ "LEVEL: 09" \c -> do
        ld [HL] $ fromIntegral $ ord c
        inc HL
    ld [HL] frameE

    ld HL $ videoStart + 23 * numCols + wellEndX + 2
    ld [HL] frameSW
    inc HL
    replicateM_ 9 do
        ld [HL] frameS
        inc HL
    ld [HL] frameSE

drawPieceStats :: Location -> Z80ASM
drawPieceStats pieces = do
    ld HL $ videoStart + 0 * numCols + 1
    ld [HL] frameNW
    inc HL
    replicateM_ 10 do
        ld [HL] frameN
        inc HL
    ld [HL] frameNE
    ld HL $ videoStart + 23 * numCols + 1
    ld [HL] frameSW
    inc HL
    replicateM_ 10 do
        ld [HL] frameS
        inc HL
    ld [HL] frameSE

    ld HL $ videoStart + 1 * numCols + 1
    ld [HL] frameW
    inc HL
    forM_ "STATISTICS" \c -> do
        ld [HL] $ fromIntegral $ ord c
        inc HL
    ld [HL] frameE

    ld DE numCols
    ld HL $ videoStart + 2 * numCols + 1
    decLoopB 21 do
        ld [HL] frameW
        add HL DE
    ld HL $ videoStart + 2 * numCols + 1 + 11
    decLoopB 21 do
        ld [HL] frameE
        add HL DE

    forM_ [0..6] \k -> do
        ld HL $ videoStart + (3 + 3 * k) * numCols - 1
        let base = pieces + k * 4 * 4 * 2
        drawLinesFrom base False 4

drawLine :: Bool -> Z80ASM
drawLine bg = do
    sla D
    rl E
    decLoopB wellWidth do
        sla D
        rl E
        when bg $ ld [HL] well
        unlessFlag NC $ ld [HL] A
        inc HL

-- | `HL`: topmost line's start
drawLinesFrom :: Location -> Bool -> Word8 -> Z80ASM
drawLinesFrom from bg n = do
    exx
    ld HL from
    exx
    drawLines bg n

-- | `HL`: topmost line's start
-- | `HL'`: address of piece data
drawLines :: Bool -> Word8 -> Z80ASM
drawLines bg n = do
    ld A block
    exx
    decLoopB n do
        ld D [HL]
        inc HL
        ld E [HL]
        inc HL
        push DE
        exx
        pop DE
        drawLine bg
        ld DE (numCols - wellWidth)
        add HL DE
        exx
    exx

drawNext :: Z80ASM
drawNext = do
    ld HL $ videoStart + 11 * numCols + wellEndX + 4
    ld [HL] frameNW
    inc HL
    replicateM_ 5 do
        ld [HL] frameN
        inc HL
    ld [HL] frameNE


    ld HL $ videoStart + 12 * numCols + wellEndX + 4
    ld [HL] frameW
    inc HL
    forM_ "NEXT:" \c -> do
        ld [HL] $ fromIntegral $ ord c
        inc HL
    ld [HL] frameE

    ld HL $ videoStart + 13 * numCols + wellEndX + 4
    ld [HL] frameW
    ld HL $ videoStart + 13 * numCols + wellEndX + 4 + 6
    ld [HL] frameE

    ld HL $ videoStart + 14 * numCols + wellEndX + 4
    ld [HL] frameW
    inc HL
    inc HL
    forM_ [block, block, block, space] \b -> do
        ld [HL] b
        inc HL
    ld [HL] frameE

    ld HL $ videoStart + 15 * numCols + wellEndX + 4
    ld [HL] frameW
    inc HL
    inc HL
    forM_ [space, space, block, space] \b -> do
        ld [HL] b
        inc HL
    ld [HL] frameE

    ld HL $ videoStart + 16 * numCols + wellEndX + 4
    ld [HL] frameSW
    inc HL
    replicateM_ 5 do
        ld [HL] frameS
        inc HL
    ld [HL] frameSE
