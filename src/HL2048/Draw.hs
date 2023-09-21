{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module HL2048.Draw
    ( clearScreen, drawScreen, drawTile, prepareGrid
    , calcMoveN, calcMoveS, calcMoveE, calcMoveW
    ) where

import HL2048
import HL2048.Input

import Z80
import Z80.Utils
import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char

clearScreen :: Locations -> Z80ASM
clearScreen MkLocs{..} = mdo
    ld HL videoStart
    ld IX screenBuf
    loop <- label
    ld [HL] space
    inc HL
    ld [IX] space
    inc IX
    ld A H
    cp 0xc4
    jp NZ loop

xoff :: Integral a => a
xoff = (numCols - 4 * (tileWidth + 3)) `div` 2

yoff :: Integral a => a
yoff = numCols * ((numRows - 4 * (tileHeight + 3)) `div` 2)

drawGrid :: Z80ASM
drawGrid = do
    ld IX $ videoStart + yoff + xoff + numCols
    drawGrid'

prepareGrid :: Locations -> Z80ASM
prepareGrid MkLocs{..} = do
    ld IX screenBuf
    drawGrid'

    ld DE $ videoStart + yoff + xoff - 1
    ld HL $ screenBuf
    ld BC $ 40 * 25
    ldir

blitGrid :: Locations -> Z80ASM
blitGrid MkLocs{..} = do
    let from = screenBuf + 1
        to = videoStart + yoff + xoff

    forM_ [1 .. 4 * (tileHeight + 3) - 1] \i -> do
        ld HL $ from + i * numCols
        ld DE $ to + i * numCols
        ld BC $ 4 * (tileWidth + 3)
        ldir

gridX, gridH, gridV :: Word8
gridX = 0x79
gridH = 0x95
gridV = 0xea

drawGrid' :: Z80ASM
drawGrid' = do
    ld D 0
    ld E $ numCols - (4 * (tileWidth + 3)) - 1
    replicateM_ 4 do
        exx
        gridSegment gridH gridX
        exx
        add IX DE
        decLoopB (tileHeight + 2) do
            exx
            gridSegment 0xfb gridV
            exx
            add IX DE
    gridSegment gridH gridX

gridSegment :: Word8 -> Word8 -> Z80ASM
gridSegment c1 c2 = do
    ld C 1 -- $ tileWidth + 3
    ld B $ 4 * (tileWidth + 3) + 1
    withLabel \loop -> mdo
        dec C
        jp NZ normal
        ld [IX] c2
        ld C $ tileWidth + 3
        jp next

        normal <- label
        ld [IX] c1

        next <- label
        inc IX
        djnz loop

-- | Pre: `IX` is top left corner
-- | Pre: `IY` is contents (2 bytes)
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

    replicateM_ (tileHeight `div` 2) do
        add IX DE
        ld [IX] 0x90 -- 0xeb
        inc IX
        decLoopB tileWidth do
            ld [IX] space
            inc IX
        ld [IX] 0x90 -- 0xea
        inc IX

    add IX DE
    ld [IX] 0x90 -- 0xeb
    inc IX
    decLoopB tileWidth do
        ldVia A [IX] [IY]
        inc IX
        inc IY
    ld [IX] 0x90 -- 0xea
    inc IX

    replicateM_ (tileHeight - ((tileHeight `div` 2) + 1)) do
        add IX DE
        ld [IX] 0x90 -- 0xeb
        inc IX
        decLoopB tileWidth do
            ld [IX] space
            inc IX
        ld [IX] 0x90 -- 0xea
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
drawScreen locs@MkLocs{..} = do
    blitGrid locs
    drawTiles locs
    pure ()

drawTiles :: Locations -> Z80ASM
drawTiles locs@MkLocs{..} = mdo
    forM_ [0..3] \i -> forM_ [0..3] \j -> skippable \next -> do
        ld BC (j * 4 + i)
        ld IX $ videoStart + yoff + (xoff + ((tileWidth + 3) * i)) + (1 + (tileHeight + 3) * j) * numCols
        call drawMovedTile
    ret

    numbers <- labelled $ db $ mconcat
      [ map (fromIntegral . ord) $ replicate (4 - length s) ' ' <> s | i <- [0..13], let s = show (2 ^ i) ]

    -- `IX` contains top left corner
    -- `BC` contains tile index in data structures
    drawMovedTile <- labelled do
        ld HL tileValues
        add HL BC
        ld A [HL]
        sub 1
        ret C

        ld IY numbers
        sla A
        sla A
        ld D 0
        ld E A
        add IY DE

        ld HL tileOffs
        add HL BC
        ld A [HL]

        call calcMoveF
        add IX DE

        jp drawTileF -- drawTileF will return for us!

    pure ()

-- | Pre: `D` is 0
-- | Pre: `A` is the amount to offset
-- | Post: `DE` is the tile delta
calcMoveN, calcMoveS, calcMoveE, calcMoveW :: Z80ASM
calcMoveE = do
    ld E A
    ret

calcMoveW = do
    neg
    ld E A
    ret Z
    ld D 0xff
    ret

calcMoveS = mdo
    ld E A
    ld HL times40
    add HL DE
    add HL DE
    ld E [HL]
    inc HL
    ld D [HL]
    ret

    times40 <- labelled $ dw [ i * numCols | i <- [0.. 4 * (tileHeight + 3)] ]
    pure ()

calcMoveN = mdo
    ld E A
    ld HL timesNeg40
    add HL DE
    add HL DE
    ld E [HL]
    inc HL
    ld D [HL]
    ret

    timesNeg40 <- labelled $ dw [ negate i * numCols | i <- [0.. 4 * (tileHeight + 3)] ]
    pure ()
