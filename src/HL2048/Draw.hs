{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module HL2048.Draw
    ( clearScreen, drawTexts, drawScreen, drawTile, prepareGrid
    , calcAnimN, calcAnimS, calcAnimE, calcAnimW
    , drawScore
    ) where

import HL2048
import HL2048.Input

import Z80
import Z80.Utils
import HL2

import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char

clearScreen :: Locations -> Z80ASM
clearScreen MkLocs{..} = do
    ld HL videoStart
    ld IX screenBuf
    ld IY doubleBuf
    withLabel \loop -> do
        ld A space
        ld [HL] A
        inc HL
        ld [IX] A
        inc IX
        ld [IY] A
        inc IY
        ld A H
        cp 0xc4
        jp NZ loop

-- | Pre: `HL` is pointer to a 24-bit unsigned int in little-endian
-- | Post: `HL` is pointer to 10-digit decimal number
-- Based on https://artemis.sh/2014/11/06/z80-assembly-binary-coded-decimal.html
toBCD24 :: Z80ASM
toBCD24 = mdo
    -- Copy input to `src`
    ld DE src
    ld BC numBytes
    ldir
    ld IX src

    -- Clear out `dst`
    clearA
    ld HL dst
    decLoopB numDigits do
        ld [HL] A
        inc HL

    decLoopB (numBytes * 8) do
        ld HL dst
        ld C numDigits
        -- Iterate through each BCD digit. If digit > 4, add 3
        withLabel \incLoop -> do
            ld A [HL]
            skippable \lessThan4 -> do
                cp 5
                jr C lessThan4
                add A 3
            ld [HL] A
            inc HL

            dec C
            jr NZ incLoop

        -- Shift SRC bits
        sla [IX]
        forM_ [1 .. numBytes - 1] \i -> do
            rl [IX + fromIntegral i]

        ld HL dst
        ld C numDigits
        withLabel \shiftLoop -> do
            ld A [HL]
            rla
            skippable \not4 -> do
                Z80.bit 4 A
                jr Z not4
                Z80.and 0x0f -- Mask out high bits, since we only want the lower 4 bits for the digit
                scf          -- Set carry if bit 4 is set
            ld [HL] A
            inc HL

            dec C
            jr NZ shiftLoop

    ld HL dst
    ret

    src <- labelled $ db $ replicate numBytes 0
    dst <- labelled $ db $ replicate numDigits 0
    pure ()
  where
    numBytes :: Num a => a
    numBytes = 3

    numDigits :: Num a => a
    numDigits = 10


drawScore :: Locations -> Z80ASM
drawScore MkLocs{..} = mdo
    ld IX $ videoStart + numCols * (numRows - 1)
    ld A 0xe0
    decLoopB numCols do
        ld [IX] A
        inc IX
    ld IX $ videoStart + numCols * (numRows - 1) + xoff - 1
    ld IY text
    text <- stringLoopB (invert "SCORE:") do
        ldVia A [IX] [IY]
        inc IX
        inc IY

    -- Clear sum
    ld HL sum
    clearA
    decLoopB 3 do
        ld [HL] A
        inc HL

    -- Sum tileValues into [HL]
    ld IX tileValues
    ld HL 0
    ld D 0
    decLoopB 16 do
        -- Look up tile value in powers-of-2 table
        ld E [IX]
        inc IX
        ld IY powers
        replicateM_ 2 $ sla E
        add IY DE

        -- Add [IY] to [HL]
        ld HL sum
        scf
        ccf
        replicateM_ 3 do
            ld A [HL]
            adc A [IY]
            inc IY
            ld [HL] A
            inc HL

    ld HL sum
    call toBCDF

    -- Print value
    ld IX $ videoStart + numCols * (numRows - 1) + xoff - 1 + 8

    -- Most to least significant digits
    ld DE 9
    add HL DE

    ld A 0 -- Have we seen a non-zero digit yet?
    decLoopB 10 $ skippable \next -> do
        ld C [HL]
        dec HL

        -- Skip leading zeroes
        Z80.or C
        cp 0
        jp Z next

        push AF
        ld A C
        add A 0xb0
        ld [IX] A
        inc IX
        pop AF
    ret

    sum <- labelled $ db [0, 0, 0]
    powers <- labelled $ db . mconcat $
      [ [ b0, b1, b2, 0 ]
      | v <- 0:[2 ^ i :: Word32 | i <- [1..17]]
      , let b0 = fromIntegral v
      , let b1 = fromIntegral (v `shiftR` 8)
      , let b2 = fromIntegral (v `shiftR` 16)
      ]
    toBCDF <- labelled toBCD24

    pure ()


drawTexts :: Locations -> Z80ASM
drawTexts MkLocs{..} = do
    ld IX $ videoStart + numCols * 0
    ld A 0xa0
    decLoopB (5 * numCols) do
        ld [IX] A
        inc IX
    printCenteredLine videoStart 1 $ invert "HOMELAB-2048"
    printCenteredLine videoStart 3 $ invert "HTTPS://GERGO.ERDI.HU/"

    let lineNum = 8

    forM_ (zip [0..] lines) \(i, line) -> mdo
        ld IX $ videoStart + numCols * (lineNum + i) + xoff + (tileWidth + 3) * 4 + 1
        ld IY text
        text <- stringLoopB line do
            ldVia A [IX] [IY]
            inc IX
            inc IY
        pure ()
  where
    lines =
      [ "   I"
      , " "
      , "   \x04"
      , "J \x01\x73\x00 L"
      , "   \x05"
      , " "
      , "   K"
      , " "
      , " "
      , " "
      , "\xd2\&ESTART"
      , " "
      , "\xd5\&NDO"
      ]

xoff :: Integral a => a
xoff = 2

yoff :: Integral a => a
yoff = numCols * (((numRows - 4 * (tileHeight + 3)) `div` 2) + 2)

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
        to = doubleBuf + 1

    forM_ [1 .. 4 * (tileHeight + 3) - 1] \i -> do
        ld HL $ from + i * numCols
        ld DE $ to + i * numCols
        ld BC $ 4 * (tileWidth + 3) - 1
        ldir

blitBuf :: Locations -> Z80ASM
blitBuf MkLocs{..} = do
    let from = doubleBuf + 1
        to = videoStart + yoff + xoff

    forM_ [1 .. 4 * (tileHeight + 3) - 1] \i -> do
        ld HL $ from + i * numCols
        ld DE $ to + i * numCols
        ld BC $ 4 * (tileWidth + 3) - 1
        ldir

gridX, gridH, gridV :: Word8
gridX = 0x79
gridH = 0x95
gridV = 0xea

drawGrid' :: Z80ASM
drawGrid' = do
    ld DE $ numCols - (4 * (tileWidth + 3)) - 1
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
    replicateM_ tileWidth do
        ld [IX] 0x91 -- 0x96
        inc IX
    ld [IX] 0x6d

    -- Left, contents, right
    ld DE $ numCols - (tileWidth + 1)

    replicateM_ (tileHeight `div` 2) do
        add IX DE
        ld [IX] 0x90 -- 0xeb
        inc IX
        replicateM_ tileWidth do
            ld [IX] space
            inc IX
        ld [IX] 0x90 -- 0xea

    add IX DE
    ld [IX] 0x90 -- 0xeb
    inc IX
    replicateM_ tileWidth do
        ldVia A [IX] [IY]
        inc IX
        inc IY
    ld [IX] 0x90 -- 0xea

    replicateM_ (tileHeight - ((tileHeight `div` 2) + 1)) do
        add IX DE
        ld [IX] 0x90 -- 0xeb
        inc IX
        replicateM_ tileWidth do
            ld [IX] space
            inc IX
        ld [IX] 0x90 -- 0xea

    -- Bottom
    add IX DE
    ld [IX] 0x6c
    inc IX
    replicateM_ tileWidth do
        ld [IX] 0x91 -- 0x95
        inc IX
    ld [IX] 0x6b
    ret

-- | Pre: `IX` is top left corner
clearTile :: Z80ASM
clearTile = do
    ld DE $ numCols - 7
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
    blitBuf locs
    ret

drawTiles :: Locations -> Z80ASM
drawTiles locs@MkLocs{..} = skippable \end -> mdo
    ld HL tileValues
    ld BC tileOffs

    forM_ [0..3] \j -> forM_ [0..3] \i -> do
        let idx = j * 4 + i
        ld IX $ doubleBuf + (1 + ((tileWidth + 3) * i)) + (1 + (tileHeight + 3) * j) * numCols
        call drawMovedTile

        unless ((i, j) == (3, 3)) do
            inc HL
            inc BC
    jp end

    numbers <- labelled $ db $ mconcat . mconcat $
      [ [ map (fromIntegral . ord) $ replicate (4 - length s) ' ' <> s | i <- [1..13], let s = show (2 ^ i) ]
      , [ map (fromIntegral . ord) $ replicate 4 c | c <- ['A'..'D'] ]
      ]

    -- `IX` contains top left corner
    -- `HL` is the pointer to the tile value
    -- `BC` is the pointer to the tile offset
    drawMovedTile <- labelled do
        -- Load tile value
        ld A [HL]
        sub 1
        ret C

        -- Tile value label
        ld IY numbers
        rla
        rla
        ld D 0
        ld E A
        add IY DE

        -- Tile offset
        ld A [BC]
        call calcAnimF
        add IX DE

        jp drawTileF -- drawTileF will return for us!

    pure ()

-- | Pre: `D` is 0
-- | Pre: `A` is the amount to offset
-- | Post: `DE` is the tile delta
calcAnimN, calcAnimS, calcAnimE, calcAnimW :: Z80ASM
calcAnimE = do
    ld E A
    ret

calcAnimW = do
    neg
    ld E A
    ret Z
    dec D
    ret

calcAnimS = mdo
    ld E A
    push IY
    ld IY times40
    add IY DE
    add IY DE
    ld E [IY]
    inc IY
    ld D [IY]
    pop IY
    ret

    times40 <- labelled $ dw [ i * numCols | i <- [0.. 4 * (tileHeight + 3)] ]
    pure ()

calcAnimN = mdo
    ld E A
    push IY
    ld IY timesNeg40
    add IY DE
    add IY DE
    ld E [IY]
    inc IY
    ld D [IY]
    pop IY
    ret

    timesNeg40 <- labelled $ dw [ negate i * numCols | i <- [0.. 4 * (tileHeight + 3)] ]
    pure ()
