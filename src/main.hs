{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Tetris

import Z80
import Data.Word
import qualified Data.ByteString as BS
import Control.Monad
import Data.Bits
import Data.Char
import Text.Printf

main :: IO ()
main = do
    BS.writeFile "snake.obj" $ asmData block
  where
    block = org 20000 do
        -- booter
        tetris
        -- drawBorder
        -- drawSnake
        _ <- loopForever $ pure ()
        pure ()

labelASCII :: Location -> [Word8]
labelASCII loc = map (+ 0x30) $ digits
  where
    -- The length of this has to be lazy in the actual value of `loc`
    digits = [fromIntegral $ (loc `div` (10 ^ i)) `mod` 10 | i <- [4, 3 .. 0]]

booter :: Z80ASM
booter = do
    db [0x00, 0x00, 0xad] -- 0 CALL
    rec
        db $ labelASCII start
        db [0x60, 0x7f, 0xff, 0x91, 0x3a]
        start <- label
    pure ()

videoStart :: Word16
videoStart = 0xc001

numCols :: (Num a) => a
numCols = 40

numRows :: (Num a) => a
numRows = 25

space :: Word8
space = 0x20
-- -- space = 0xfb

wall :: Word8
wall = 0xfb
-- -- wall = 0xa0

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
