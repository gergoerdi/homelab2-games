{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module TrafficJam.Main (game) where

import TrafficJam
import TrafficJam.Input
import TrafficJam.Draw

import HL2
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
    drawFrame
    call testScreenF
    loopForever $ pure ()
    testScreenF <- labelled $ testScreen locs

    player <- labelled $ db $ map pos [15]
    positions <- labelled $ db $ map pos $ take 35 . (<> repeat 0) $
      [ 0, 1, 3, 4, 8, 10, 11, 18, 22, 23, 24, 26, 30, 32 ]
    sizes <- labelled $ db $ take 35 . (<> repeat 0) $
      [ 3, 3, 2, 2, 2, 1,  2,  2,  2,  1,  2,  2,  2,  2  ]
    orients <- labelled $ db $ take 35 . (<> repeat 0) $
      [ 6, 6, 6, 1, 6, 0,  6,  1,  6,  0,  1,  1,  1,  1  ]
    pure ()
  where
    pos :: Word8 -> Word8
    pos i = x `shiftL` 4 .|. y
      where
        (y, x) = i `divMod` 6

xoff = 9
yoff = 3

drawFrame :: Z80ASM
drawFrame = do
    ld HL $ videoStart + yoff * numCols + xoff + 1
    ld A 0x99
    decLoopB (6 * 3) do
        ld [HL] A
        inc HL
    ld HL $ videoStart + (yoff + 6 * 3 + 1) * numCols + xoff + 1
    ld A 0x92
    decLoopB (6 * 3) do
        ld [HL] A
        inc HL

    ld HL $ videoStart + (yoff + 1) * numCols + xoff
    ld IX $ videoStart + (yoff + 1) * numCols + xoff + 6 * 3 + 1
    ld DE 40
    decLoopB (6 * 3) do
        ld [HL] 0xee
        add HL DE
        ld [IX] 0xe7
        add IX DE

    ld IX $ videoStart + (yoff + 1 + 2 * 3 - 1) * numCols + xoff + 6 * 3 + 1
    ld DE 40
    ld [IX] 0x7e
    add IX DE
    decLoopB 3 do
        ld [IX] space
        add IX DE
    ld [IX] 0x7f

-- | Pre: `IX` is top left corner
paintPlayer :: Z80ASM
paintPlayer = do
    ld DE $ numCols - 5
    ld [IX] tl
    inc IX
    replicateM_ 4 do
        ld [IX] top
        inc IX
    ld [IX] tr
    add IX DE
    ld [IX] left
    inc IX
    replicateM_ 4 do
        ld [IX] space
        inc IX
    ld [IX] right
    add IX DE
    ld [IX] bl
    inc IX
    replicateM_ 4 do
        ld [IX] bottom
        inc IX
    ld [IX] br

    ret

  where
    top = 0x95
    bottom = 0x96
    left = 0xeb
    right = 0xee
    tl = 0x6a
    bl = 0x69
    tr = 0x13
    br = 0x11

-- | Pre: `C` is car index
drawCar :: Locations -> Z80ASM
drawCar MkLocs{..} = do
    ld HL sizes
    ld B 0
    add HL BC
    ld A [HL]

    -- Size of 0 means no car at that index
    cp 0
    ret Z

    ld IY positions
    add IY BC
    ld A [IY]

    ret

-- | Pre: `C` is player's position
drawPlayer :: Z80ASM
drawPlayer = mdo
    ld IX $ videoStart + (yoff + 1) * numCols + xoff + 1

    -- Add y * 3 * numCols to IX
    ld DE (3 * numCols)
    push BC
    ld A C
    Z80.and 0x0f
    skippable \end -> loopForever do
        jp Z end
        add IX DE
        dec A

    -- Add x * 3 to IX
    ld A C
    replicateM_ 4 $ srl A
    ld C A
    add A C
    add A C
    ld D 0
    ld E A
    add IX DE

    paintPlayer -- paintPlayer returns for us

testScreen :: Locations -> Z80ASM
testScreen MkLocs{..} = mdo
    -- forM_ (zip [yoff + 1..] lines) \(i, s) -> do
    --     ld HL $ videoStart + i * numCols + xoff + 1
    --     forM_ s \c -> do
    --         ld [HL] c
    --         inc HL
    ldVia A C [player]
    drawPlayer
    ret
  where
    lines = mconcat
      [ [ [ tl, top, tr,       btl, btop, btr ]
        , [ left, arrt, right, bleft, bmid, bright ]
        , [ left, arrv, right, bbl, bbottom, bbr ]

        , [ left, arrv, right ]
        , [ left, arrb, right ]
        , [ bl, bottom, br ]
        ]
      , [ [ tl, top, tr,        tl',   top', top',        top',    top',    tr',    tl, top, top,         top, top, tr ]
        , [ left, space, right, left', arrl, arrh,        arrh,    arrr,    right', left, space, space,   space, space, right ]
        , [ left, space, right, bl',   bottom', bottom',  bottom', bottom', br'   , bl, bottom, bottom,   bottom, bottom, br ]

        , [ left, space, right,   tl, top, top,        top, top, tr ]
        , [ left, space, right,   left, space, space,  space, space, right]
        , [ left, space, right,   bl, bottom, bottom,  bottom, bottom, br ]

        , [ left, space, right ]
        , [ left, space, right ]
        , [ bl, bottom, br ]
        ]
      , [ [ tl, top, top, top, top, tr ]
        , [ left, space, space, space, space, right ]
        , [ bl, bottom, bottom, bottom, bottom, br ]
        ]
      ]

    -- tl = 0x7c -- 0xf2
    -- tr = 0x7a -- 0xf1
    -- bl = 0x7b -- 0xf0
    -- br = 0x7d -- 0xef
    -- top = 0xf9 -- 0x9c
    -- bottom = 0x9c -- 0xf9
    -- left = 0xe4
    -- right = 0xfa

    -- tl = 0xf2
    -- tr = 0xf1
    -- bl = 0xf0
    -- br = 0xef
    -- top = 0x9c
    -- bottom = 0xf9
    -- left = 0xfa
    -- right = 0xe4

    -- tl = 0xf5
    -- tr = 0xf6
    -- bl = 0x7b
    -- br = 0x7d
    -- top = 0xf9
    -- bottom = 0xa0 -- 0x9c
    -- left = 0xe4
    -- right = 0xfa

    -- tl = 0xf5
    -- tr = 0xf6
    -- bl = 0xf7
    -- br = 0xf8
    -- top = 0xf9
    -- bottom = 0x9c
    -- left = 0xe4
    -- right = 0xfa

    -- tl = 0x6e -- 0x18
    -- tr = 0x19
    -- bl = 0x19
    -- br = 0x18
    -- top = 0x93
    -- bottom = 0x98
    -- left = 0xe8
    -- right = 0xed

    tl = 0x6e
    tr = 0x6d
    bl = 0x6c
    br = 0x6b
    bl2 = 0x69
    br2 = 0x67
    -- tl = 0x6a
    -- tr = 0x68
    -- bl = 0x69
    -- br = 0x67
    top = 0x95
    bottom = 0x96
    left = 0xea
    right = 0xeb

    arrt = 0x8c
    arrb = 0x8a
    arrv = 0x90
    -- arrl = 0x8b
    -- arrr = 0x8d
    -- arrh = 0x91
    arrl = space
    arrr = space
    arrh = space

    btl = 0xf2
    btop = 0x9c
    btr = 0xf1
    bbl = 0xf0
    bbr = 0xef
    bleft = 0xfa
    bright = 0xe4
    bbottom = 0xf9
    bmid = 0x84

    -- tr' = 0x13
    -- right' = 0xee
    -- br' = 0x11
    -- tl' = 0x10
    -- bl' = 0x12
    -- left' = 0xe7

    -- -- tr' = 0x16
    -- -- tl' = 0x15
    -- -- left' = 0xea
    -- -- right' = 0xeb
    -- -- bl' = 0x17
    -- -- br' = 0x14
    -- -- top' = 0x92
    -- -- bottom' = 0x99
    -- -- tl' = 0x18
    -- -- tr' = 0x19
    -- -- bl' = 0x19
    -- -- br' = 0x18
    -- -- tl' = 0xf2
    -- tl' = 0xf2
    -- -- tr' = 0xf1
    -- tr' = 0x7a
    -- bl' = 0xf0
    -- -- br' = 0xef
    -- br' = 0x7d
    -- left' = 0xfa
    -- right' = 0xfa
    -- -- left' = 0x1b -- 0xfa
    -- -- right' = 0xe4
    -- top' = 0xf9
    -- bottom' = 0x9c

    -- tl' = 0x15
    -- left' = 0xea
    -- bl' = 0x17
    -- tr' = 0x19
    -- right' = 0xee
    -- br' = 0x18

    tl' = 0x6a
    bl' = 0x69
    left' = 0xeb
    tr' = 0x13
    br' = 0x11
    -- right' = 0x6f
    right' = 0xee

    -- bottom' = 0x99
    -- top' = 0x92
    bottom' = 0x96
    top' = 0x95
