{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Z80
import Data.Word
import qualified Data.ByteString as BS
import Control.Monad
import Data.Bits
import Data.Char

main :: IO ()
main = do
    BS.writeFile "snake.obj" $ asmData block
  where
    block = org 0x50_00 do
        clearScreen
        drawTetris
        -- drawBorder
        -- drawSnake
        _ <- loopForever $ pure ()
        pure ()

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

drawSnake :: Z80ASM
drawSnake = do
    ld HL (videoStart + 4 * numCols + 10)
    ld [HL] 0x6e
    inc HL
    decLoopB 10 do
        ld [HL] 0x91
        inc HL
    ld [HL] 0x8d
    ld HL (videoStart + 5 * numCols + 10)
    ld [HL] 0x90

    ld HL (videoStart + 4 * numCols + 32)
    ld [HL] 0x75

wellWidth :: Num a => a
wellWidth = 10

wellHeight :: Num a => a
wellHeight = 20

block :: Word8
block = 0x77

state :: [Word16]
state = [ 0b00000_00000
        , 0b00000_00000
        , 0b00000_00000
        , 0b00011_00000
        , 0b00001_10000
        , 0b00000_00000
        , 0b00000_00000
        , 0b00000_00000
        , 0b00000_00000
        , 0b00000_00000
        , 0b00000_00000
        , 0b00000_00000
        , 0b00000_00000
        , 0b00000_00000
        , 0b00000_00000
        , 0b00000_00000
        , 0b00011_11000
        , 0b01001_11100
        , 0b11111_11100
        , 0b11111_11110
        ]

wellStartX = (numCols - (wellWidth + 2)) `div` 2
wellEndX = wellStartX + wellWidth + 1
wellStartY = 3
wellEndY = wellStartY + wellHeight


drawTetris :: Z80ASM
drawTetris = do
    ld HL $ videoStart + wellStartY * numCols + wellStartX
    ld DE numCols
    decLoopB wellHeight do
        ld [HL] wall
        add HL DE
    ld HL $ videoStart + wellStartY * numCols + wellEndX
    decLoopB wellHeight do
        ld [HL] wall
        add HL DE
    ld HL $ videoStart + wellEndY * numCols + wellStartX
    decLoopB (wellWidth + 2) do
        ld [HL] wall
        inc HL

    forM_ (zip [1..] (reverse state)) \(i, x) -> do
        ld HL $ videoStart + (wellEndY - i) * numCols + wellStartX + 1
        forM_ (reverse [0..9]) \j -> do
            ld [HL] $ if x `testBit` j then block else well
            inc HL

    ld HL $ videoStart + 11 * numCols + wellEndX + 4
    ld [HL] 0x6e
    inc HL
    replicateM_ 5 do
        ld [HL] 0x96
        inc HL
    ld [HL] 0x6d


    ld HL $ videoStart + 12 * numCols + wellEndX + 4
    ld [HL] 0xea
    inc HL
    forM_ "NEXT:" \c -> do
        ld [HL] $ fromIntegral $ ord c
        inc HL
    ld [HL] 0xeb

    ld HL $ videoStart + 13 * numCols + wellEndX + 4
    ld [HL] 0xea
    ld HL $ videoStart + 13 * numCols + wellEndX + 4 + 6
    ld [HL] 0xeb

    ld HL $ videoStart + 14 * numCols + wellEndX + 4
    ld [HL] 0xea
    inc HL
    inc HL
    forM_ [block, block, block, space] \b -> do
        ld [HL] b
        inc HL
    ld [HL] 0xeb

    ld HL $ videoStart + 15 * numCols + wellEndX + 4
    ld [HL] 0xea
    inc HL
    inc HL
    forM_ [space, space, block, space] \b -> do
        ld [HL] b
        inc HL
    ld [HL] 0xeb

    ld HL $ videoStart + 16 * numCols + wellEndX + 4
    ld [HL] 0x6c
    inc HL
    replicateM_ 5 do
        ld [HL] 0x95
        inc HL
    ld [HL] 0x6b


    ld HL $ videoStart + 1 * numCols + wellEndX
    ld [HL] 0xea
    ld HL $ videoStart + 1 * numCols + wellStartX
    ld [HL] 0xeb
    inc HL
    forM_ "LINES: 014" \c -> do
        ld [HL] $ fromIntegral $ ord c
        inc HL

    ld HL $ videoStart + 0 * numCols + wellStartX
    ld [HL] 0x6e
    inc HL
    replicateM_ wellWidth $ do
        ld [HL] 0x96
        inc HL
    ld [HL] 0x6d

    ld HL $ videoStart + 2 * numCols + wellStartX
    ld [HL] 0x6c
    inc HL
    replicateM_ wellWidth $ do
        ld [HL] 0x95
        inc HL
    ld [HL] 0x6b



    ld HL $ videoStart + 3 * numCols + wellEndX + 3
    ld [HL] 0x6e
    inc HL
    replicateM_ 8 do
        ld [HL] 0x96
        inc HL
    ld [HL] 0x6d

    ld HL $ videoStart + 4 * numCols + wellEndX + 3
    ld [HL] 0xea
    inc HL
    forM_ "SCORE:  " \c -> do
        ld [HL] $ fromIntegral $ ord c
        inc HL
    ld [HL] 0xeb

    ld HL $ videoStart + 5 * numCols + wellEndX + 3
    ld [HL] 0xea
    inc HL
    forM_ "00000000" \c -> do
        ld [HL] $ fromIntegral $ ord c
        inc HL
    ld [HL] 0xeb
    ld HL $ videoStart + 6 * numCols + wellEndX + 3
    ld [HL] 0x6c
    inc HL
    replicateM_ 8 do
        ld [HL] 0x95
        inc HL
    ld [HL] 0x6b



    ld HL $ videoStart + 21 * numCols + wellEndX + 2
    ld [HL] 0x6e
    inc HL
    replicateM_ 9 do
        ld [HL] 0x96
        inc HL
    ld [HL] 0x6d

    ld HL $ videoStart + 22 * numCols + wellEndX + 2
    ld [HL] 0xea
    inc HL
    forM_ "LEVEL: 09" \c -> do
        ld [HL] $ fromIntegral $ ord c
        inc HL
    ld [HL] 0xeb

    ld HL $ videoStart + 23 * numCols + wellEndX + 2
    ld [HL] 0x6c
    inc HL
    replicateM_ 9 do
        ld [HL] 0x95
        inc HL
    ld [HL] 0x6b


    ld HL $ videoStart + 0 * numCols + 1
    ld [HL] 0x6e
    inc HL
    replicateM_ 10 do
        ld [HL] 0x96
        inc HL
    ld [HL] 0x6d
    ld HL $ videoStart + 23 * numCols + 1
    ld [HL] 0x6c
    inc HL
    replicateM_ 10 do
        ld [HL] 0x95
        inc HL
    ld [HL] 0x6b

    ld HL $ videoStart + 1 * numCols + 1
    ld [HL] 0xea
    inc HL
    forM_ "STATISTICS" \c -> do
        ld [HL] $ fromIntegral $ ord c
        inc HL
    ld [HL] 0xeb

    ld DE numCols
    ld HL $ videoStart + 2 * numCols + 1
    decLoopB 21 do
        ld [HL] 0xea
        add HL DE
    ld HL $ videoStart + 2 * numCols + 1 + 11
    decLoopB 21 do
        ld [HL] 0xeb
        add HL DE

    let shapes =
          [ [ [block, block, block, space]
            , [space, block, space, space]
            ]
          , [ [block, block, block, space]
            , [space, space, block, space]
            ]
          , [ [block, block, space, space]
            , [space, block, block, space]
            ]
          , [ [space, block, block, space]
            , [space, block, block, space]
            ]
          , [ [space, block, block, space]
            , [block, block, space, space]
            ]
          , [ [block, block, block, space]
            , [block, space, space, space]
            ]
          , [ [block, block, block, block]
            , [space, space, space, space]
            ]
          ]

    forM_ (zip [0..] shapes) \ (k, shape) -> do
        forM_ (zip [0..] shape) \ (i, bs) -> do
            ld HL $ videoStart + (3 + 3 * k + i) * numCols + 3
            forM_ bs \b -> do
                ld [HL] b
                inc HL
