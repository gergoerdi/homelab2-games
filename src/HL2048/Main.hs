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

    clearScreen locs
    prepareGrid locs

    loopForever do
        ld HL anim
        -- ld [HL] (tileWidth + 3)
        ld [HL] (tileHeight + 3)

        loopForever do
            drawScreen locs
            halt

    loopForever $ pure ()
    drawTileF <- labelled drawTile
    clearTileF <- labelled clearTile
    anim <- labelled $ db [0]
    animSub <- labelled $ db [0]
    screenBuf <- labelled $ resb $ 40 * 25
    pure ()

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

tileHeight :: Num a => a
tileHeight = 2

tileWidth :: Num a => a
tileWidth = 3

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
    ld DE $ videoStart + yoff + xoff + numCols
    ld HL $ screenBuf + numCols + 1

    replicateM_ (4 * (tileHeight + 3) - 1) $ do
        ld BC $ 4 * (tileWidth + 3)
        ldir
        ld BC $ numCols - (4 * (tileWidth + 3))
        add HL BC

        push HL
        push DE
        pop HL
        add HL BC
        push HL
        pop DE
        pop HL

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
            gridSegment 0x00 gridV
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
drawTiles locs@MkLocs{..} = skippable \end -> mdo
    forM_ [0..3] \i -> forM_ [0..3] \j -> skippable \next -> do
        ld BC (j * 4 + i)
        ld IX $ videoStart + yoff + (xoff + ((tileWidth + 3) * i)) + (1 + (tileHeight + 3) * j) * numCols
        call moveTile

    skippable \noAnim -> mdo
        ld HL anim
        ld A [HL]
        cp 0
        jp Z noAnim
        dec A
        ld [HL] A

        forM_ [0..3] \i -> forM_ [0..3] \j -> skippable \next -> mdo
            -- Apply speed
            ld HL $ tileOffs + j * 4 + i
            ld A [tileSpeeds + j * 4 + i]

            add A [HL]
            ld [HL] A


        ld HL animSub
        ld A [HL]
        sub 1
        jp NC noNewFrame
        ld A 5
        noNewFrame <- label
        ld [HL] A

    jp end

    numbers <- labelled $ db $ mconcat . map mconcat $
      [ [ [space, fromIntegral (ord '0') + i, space, space] | i <- [1..9] ]
      , [ [fromIntegral (ord '1'), fromIntegral (ord '0') + i, space, space] | i <- [0..8] ]
      ]

    tileValues <- labelled $ db
      [ 12, 0, 0, 0
      , 1, 0, 1, 0
      , 1, 0, 0, 1
      , 1, 2, 0, 0
      ]

    -- `IX` contains top left corner
    -- `BC` contains tile index in data structures
    moveTile <- labelled do
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

        -- -- Rightwards move
        -- ld E A
        -- add IX DE

        -- -- Leftwards move: negate DE
        -- neg
        -- ld E A
        -- skippable \isZero -> do
        --     jp Z isZero
        --     ld D 0xff
        -- add IX DE

        -- -- Downwards move: multiply DE by 40 using a lookup table
        -- ld E A
        -- ld HL times40
        -- add HL DE
        -- add HL DE
        -- ld E [HL]
        -- inc HL
        -- ld D [HL]
        -- add IX DE

        -- Upwards move: multiply DE by -40 using a lookup table
        ld E A
        ld HL timesNeg40
        add HL DE
        add HL DE
        ld E [HL]
        inc HL
        ld D [HL]
        add IX DE

        jp drawTileF -- drawTileF will return for us!


    -- -- Test: right move
    -- tileSpeeds <- labelled $ db
    --   [ 1, 0, 0, 0
    --   , 3, 0, 1, 0
    --   , 3, 0, 0, 0
    --   , 2, 2, 0, 0
    --   ]

    -- -- Test: left move
    -- tileSpeeds <- labelled $ db
    --   [ 0, 0, 0, 0
    --   , 0, 0, 2, 0
    --   , 0, 0, 0, 3
    --   , 0, 1, 0, 0
    --   ]

    -- -- Test: down move
    -- tileSpeeds <- labelled $ db
    --   [ 1, 0, 0, 0
    --   , 1, 0, 2, 0
    --   , 1, 0, 0, 1
    --   , 0, 0, 0, 0
    --   ]

    -- Test: up move
    tileSpeeds <- labelled $ db
      [ 0, 0, 0, 0
      , 0, 0, 1, 0
      , 1, 0, 0, 2
      , 1, 3, 0, 0
      ]

    tileOffs <- labelled $ db
      [ 0, 0, 0, 0
      , 0, 0, 0, 0
      , 0, 0, 0, 0
      , 0, 0, 0, 0
      ]

    times40 <- labelled $ dw [ i * numCols | i <- [0.. 4 * (tileHeight + 3)] ]
    timesNeg40 <- labelled $ dw [ negate i * numCols | i <- [0.. 4 * (tileHeight + 3)] ]

    pure ()
