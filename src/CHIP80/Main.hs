{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module CHIP80.Main (game) where

import HL2
import CHIP80.CPU
import CHIP80.Font
import CHIP80.HL2.Input
import CHIP80.HL2.Video
import LFSR
import Z80.ZX0

import Z80
import Z80.Utils
import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char
import qualified Data.ByteString as BS
import Data.List (sortBy, groupBy, intercalate)
import Data.Function (on)
import Data.Default

-- | Pre: `IX` contains address of compressed program
game :: Location -> Z80ASM
game baseAddr = mdo
    call drawUI

    -- Uncompress program into CHIP-8 RAM
    call setup

    push IX
    pop HL
    ld DE $ baseAddr + 0x200
    call uncompress

    run baseAddr

    drawUI <- labelled drawUI_
    setup <- labelled $ setup_ baseAddr
    uncompress <- labelled standardFwd

    pure ()

drawUI_ :: Z80ASM
drawUI_ = mdo
    -- Clear screen
    ld HL videoStart
    withLabel \loop -> do
        ld [HL] 0x20
        inc HL
        ld A H
        cp 0xc4
        jp NZ loop

    ld HL $ videoStart + 4 + (3 - 1) * 40 - 1
    ld [HL] 0x6e
    inc HL
    ld A 0x96
    decLoopB 32 do
        ld [HL] A
        inc HL
    ld [HL] 0x6d

    ld HL $ videoStart + 4 + (3 + 16) * 40 - 1
    ld [HL] 0x6c
    inc HL
    ld A 0x95
    decLoopB 32 do
        ld [HL] A
        inc HL
    ld [HL] 0x6b

    ld HL $ videoStart + 4 + 3 * 40 - 1
    decLoopB 16 do
        ld [HL] 0xeb
        ld DE 33
        add HL DE
        ld [HL] 0xea
        ld DE 7
        add HL DE

    -- Draw main UI
    ld HL $ videoStart + 40
    ld DE banner
    skippable \end -> loopForever do
        ld A [DE]
        Z80.and A
        jp Z end
        ld [HL] A
        inc HL
        inc DE
    ld HL $ videoStart + (3 + 16 + 1 + 1) * 40
    forM_ keyss \keys -> do
        ld DE keys
        skippable \end -> loopForever do
            ld A [DE]
            Z80.and A
            jp Z end
            ld [HL] A
            inc HL
            inc DE
        ld DE (40 - 10) -- 4 * 4 + 1)
        add HL DE
    ld HL $ videoStart + (3 + 16 + 1 + 1) * 40 + 20
    ld DE reset
    skippable \end -> loopForever do
        ld A [DE]
        Z80.and A
        jp Z end
        ld [HL] A
        inc HL
        inc DE
    ret
    banner <- labelled $ db $ (++ [0]) $ map (fromIntegral . ord . toUpper) $ invert "   CHIP-80     https://gergo.erdi.hu/   "
    keyss <- mapM (labelled . db . (++ [0]) . map (fromIntegral . ord . toUpper)) $
      -- let rows = [ [ ('1', '1'), ('2', '2'), ('3', '3'), ('C', '4') ]
      --            , [ ('4', 'q'), ('5', 'W'), ('6', 'e'), ('D', 'r') ]
      --            , [ ('7', 'a'), ('8', 's'), ('9', 'd'), ('E', 'f') ]
      --            , [ ('A', 'z'), ('0', 'x'), ('B', 'c'), ('F', 'v') ]
      --            ]
      -- in [ intercalate " " [ [sym, ' ', invert1 key] | (sym, key) <- row ] | row <- rows ]
      let rows = [ ("123C", "1234")
                 , ("456D", "QWER")
                 , ("789E", "ASDF")
                 , ("A0BF", "ZXCV")
                 ]
      in [ sym ++ "  " ++ invert key | (sym, key) <- rows ]
    reset <- labelled . db . (++ [0]) . map (fromIntegral . ord . toUpper) $
        invert "RUN/BRK" <> ": Change game"
    pure ()

setup_ :: Location -> Z80ASM
setup_ baseAddr = mdo
    -- Zero out CHIP-8 RAM
    ld DE baseAddr
    ld A 0
    decLoopB 16 do
        push BC
        decLoopB 256 do
            ld [DE] A
            inc DE
        pop BC

    -- Load hex font
    ld DE baseAddr
    ld HL hex
    ld BC $ 16 * 8
    ldir
    ret

    hex <- labelled $ db font
    pure ()

run :: Location -> Z80ASM
run baseAddr = mdo
    let vidBuf = baseAddr - 256
        keyBuf = vidBuf - 16

    ld IY $ baseAddr + 0x200
    loopForever do
        call cpu
        ld HL lastFrame
        ld A [0x403f]
        cp [HL]
        unlessFlag Z do
            ld [lastFrame] A
            call scanKeys
            call newFrame

    timer <- labelled $ db [0]
    lastFrame <- labelled $ db [0]
    waitForFrame <- labelled $ db [0]

    let platform = Platform{ vidAddr = vidBuf, .. }
    cpu <- labelled $ cpu_ def platform
    newFrame <- labelled $ newFrame_ platform
    rnd <- labelled $ dw [0xf00f]
    lfsrDE <- labelled lfsr10

    clearScreen <- labelled do
        ld HL $ videoStart + 4 + 40 * 3
        ld DE 8
        decLoopB 16 do
            ld C B
            decLoopB 32 do
                ld [HL] 0x20
                inc HL
            add HL DE
            ld B C
        ret

    spritePre <- labelled do
        ld [spriteX] A
        push AF
        ldVia A [spriteY] C
        ldVia A [spriteH] B
        pop AF
        ret

    spritePost <- label
    ld A 0x00
    spriteX <- subtract 1 <$> label
    ld C 0x00
    spriteY <- subtract 1 <$> label
    ld B 0x00
    spriteH <- subtract 1 <$> label
    drawSprite vidBuf

    -- Scan the keyboard and write its state to the 16 bytes starting at `keyBuf`
    scanKeys <- labelled $ scanKeys_ keyBuf
    pure ()

charmapHL4 :: [Word8]
charmapHL4 =
    [ 0x00 -- 00_00
    , 0x1d -- 00_01
    , 0x1e -- 00_10
    , 0x12 -- 00_11
    , 0x1c -- 01_00
    , 0xea -- 01_01
    , 0x19 -- 01_10
    , 0x18 -- 01_11
    , 0x1b -- 10_00
    , 0x1a -- 10_01
    , 0xd5 -- 10_10
    , 0x15 -- 10_11
    , 0x1f -- 11_00
    , 0x17 -- 11_01
    , 0x16 -- 11_10
    , 0xff -- 11_11
    ]
