{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Tetris (tetris, pieces) where

import Z80
import Data.Word
import qualified Data.ByteString as BS
import Control.Monad
import Data.Bits
import Data.Char
import Text.Printf
import Data.Semigroup (stimes)

tetris :: Z80ASM
tetris = do
    clearScreen
    rec drawTetris lpieces
        lpieces <- labelled $ db pieces
    pure ()

padTo :: Int -> a -> [a] -> [a]
padTo n x0 [] = replicate n x0
padTo n x0 (x:xs) = x : padTo (n-1) x0 xs

pieces :: [Word8]
pieces = concatMap (concatMap encode) [t, j, z, o, s, l, i]
  where
    normalize :: [String] -> [String]
    normalize = map (padTo 4 ' ') . padTo 4 ""

    encode :: [String] -> [Word8]
    encode = map ((`shiftL` 2) . stringToByte) . normalize

    stringToByte :: String -> Word8
    stringToByte = foldl (\b c -> let b' = b `shiftL` 1 in if c == ' ' then b' else setBit b' 0) 0x00

    t =
      [ [ "***"
        , " * "
        ]
      , [ " * "
        , " **"
        , " * "
        ]
      , [ " * "
        , "***"
        ]
      , [ " *"
        , "**"
        , " *"
        ]
      ]

    j =
      [ [ "***"
        , "  *"
        ]
      , [ " **"
        , " * "
        , " * "
        ]
      , [ "*  "
        , "***"
        ]
      , [ " *"
        , " *"
        , "**"
        ]
      ]
    l = map (map reverse) j

    z = stimes 2 $
      [ [ "** "
        , " **"
        ]
      , [ " *"
        , "**"
        , "* "
        ]
      ]
    s = map (map reverse) z

    o = stimes 4 $
      [ [ "**"
        , "**"
        ]
      ]

    i = stimes 2 $
      [ [ "****"
        ]
      , [ "*"
        , "*"
        , "*"
        , "*"
        ]
      ]

          -- [ [ [block, block, block, space]
          --   , [space, block, space, space]
          --   ]
          -- , [ [block, block, block, space]
          --   , [space, space, block, space]
          --   ]
          -- , [ [block, block, space, space]
          --   , [space, block, block, space]
          --   ]
          -- , [ [space, block, block, space]
          --   , [space, block, block, space]
          --   ]
          -- , [ [space, block, block, space]
          --   , [block, block, space, space]
          --   ]
          -- , [ [block, block, block, space]
          --   , [block, space, space, space]
          --   ]
          -- , [ [block, block, block, block]
          --   , [space, space, space, space]
          --   ]
          -- ]

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

frameNW, frameN, frameNE, frameW, frameE, frameSW, frameS, frameSE :: Word8
frameNW = 0x6e
frameN  = 0x96
frameNE = 0x6d
frameW  = 0xea
frameE  = 0xeb
frameSW = 0x6c
frameS  = 0x95
frameSE = 0x6b

drawTetris :: Location -> Z80ASM
drawTetris pieces = do
    drawWell
    drawNext
    drawLevel
    drawPieceStats pieces
    drawLines
    drawScore

    forM_ (zip [1..] (reverse state)) \(i, x) -> do
        ld HL $ videoStart + (wellEndY - i) * numCols + wellStartX + 1
        forM_ (reverse [0..9]) \j -> do
            ld [HL] $ if x `testBit` j then block else well
            inc HL

drawWell :: Z80ASM
drawWell = do
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

drawLines :: Z80ASM
drawLines = do
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
        let base = pieces + k * 4 * 4
        forM_ [0..3] \row -> do
            let addr = base + row
            ld HL $ videoStart + (3 + 3 * k + row) * numCols
            ld A [addr]
            rec
                loopForever do
                    sla A
                    rec
                        jr C drawThis
                        jr NZ next
                        jr end
                        drawThis <- labelled $ ld [HL] block
                        next <- labelled $ inc HL
                    pure ()
                end <- label
            pure ()

                -- unlessFlag NC $ ld [HL] block
                -- inc HL
                -- cp 0
                -- jr NZ lab
                -- pure ()


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
