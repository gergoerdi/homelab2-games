{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module HL2048.Main (game) where

import HL2048

import Z80
import Z80.Utils
import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char

game :: Z80ASM
game = mdo
    let locs = MkLocs{..}

    clearScreen
    drawScreen locs
    loopForever do
        replicateM_ 5 halt
        drawScreen locs

    loopForever $ pure ()
    drawTileF <- labelled drawTile
    clearTileF <- labelled clearTile
    pure ()

clearScreen :: Z80ASM
clearScreen = mdo
    ld HL videoStart
    loop <- label
    ld [HL] space
    inc HL
    ld A H
    cp 0xc4
    jp NZ loop

tileHeight :: Num a => a
tileHeight = 3

tileWidth :: Num a => a
tileWidth = 5

drawGrid :: Z80ASM
drawGrid = do
    let xoff = (numCols - 4 * (tileWidth + 3)) `div` 2
    let (lo, hi) = wordBytes $ xoff * 2 + 1
    ld D hi
    ld E lo

    -- Horizontal lines
    ld IX $ videoStart + fromIntegral xoff
    decLoopB 4 do
        exx
        decLoopB (4 * (tileWidth + 3) - 1) do
            ld [IX] 0x95
            inc IX
        exx
        add IX DE
        replicateM_ (tileHeight + 2) do
            exx
            decLoopB (4 * (tileWidth + 3) - 1) do
                ld [IX] 0x00
                inc IX
            exx
            add IX DE
    exx
    decLoopB (4 * (tileWidth + 3) - 1) do
        ld [IX] 0x95
        inc IX

    ld IX $ videoStart + fromIntegral xoff - 1 + numCols
    ld D 0
    ld E $ tileWidth + 3
    exx

    ld D 0
    ld E $ 2 * fromIntegral xoff - (tileWidth + 3)

    replicateM_ 4 do
        decLoopB (tileHeight + 3) do
            exx
            decLoopB 5 do
                ld [IX] 0xea
                add IX DE
            exx
            add IX DE

    ld IX $ videoStart + fromIntegral xoff - 1
    ld D 0
    ld E (tileWidth + 3)
    exx
    let yskip = numCols * (tileHeight + 2) - (tileWidth + 3) + 2 * xoff
    let (lo, hi) = wordBytes yskip
    ld D hi
    ld E lo
    decLoopB 5 do
        exx
        decLoopB 5 do
            ld [IX] 0x79
            add IX DE
        exx
        add IX DE
    pure ()

-- | Pre: `IX` is top left corner
-- | Pre: `IY` is contents (5 bytes)
drawTile :: Z80ASM
drawTile = do
    -- Top
    ld [IX] 0x6e
    inc IX
    decLoopB tileWidth do
        ld [IX] 0x91 -- 0x96
        inc IX
    ld [IX] 0x6d
    inc IX

    -- Left, contents, right
    ld D 0
    ld E (numCols - (tileWidth + 2))

    replicateM_ ((tileHeight - 1) `div` 2) do
        add IX DE
        ld [IX] 0xeb
        inc IX
        decLoopB tileWidth do
            ld [IX] space
            inc IX
        ld [IX] 0xea
        inc IX

    add IX DE
    ld [IX] 0xeb
    inc IX
    decLoopB tileWidth do
        ld [IX] space -- TODO: contents
        inc IX
    ld [IX] 0xea
    inc IX

    replicateM_ ((tileHeight - 1) `div` 2) do
        add IX DE
        ld [IX] 0xeb
        inc IX
        decLoopB tileWidth do
            ld [IX] space
            inc IX
        ld [IX] 0xea
        inc IX

    -- Bottom
    add IX DE
    ld [IX] 0x6c
    inc IX
    decLoopB tileWidth do
        ld [IX] 0x91 -- 0x95
        inc IX
    ld [IX] 0x6b
    ret

-- | Pre: `IX` is top left corner
clearTile :: Z80ASM
clearTile = do
    ld D 0
    ld E $ numCols - 7
    decLoopB 5 do
        exx
        decLoopB 7 do
            ld [IX] space
            inc IX
        exx
        add IX DE
    ret

drawScreen :: Locations -> Z80ASM
drawScreen MkLocs{..} = skippable \end -> mdo
    drawGrid

    let xoff = (numCols - 4 * (tileWidth + 3)) `div` 2
        yoff = 1

    ld D 0
    ld HL dx
    ld E [HL]

    -- ld IX $ videoStart + xoff + yoff * numCols
    -- add IX DE
    -- push DE
    -- call clearTileF
    -- pop DE

    ld A E
    add A 1
    ld E A
    ld HL dx
    ld [HL] E
    ld IX $ videoStart + xoff + yoff * numCols
    add IX DE
    call drawTileF

    jp end

    dx <- labelled $ db [0]
    pure ()
