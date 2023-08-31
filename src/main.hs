{-# LANGUAGE NumericUnderscores, BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Z80
import Data.Word
import qualified Data.ByteString as BS
import Control.Monad

main :: IO ()
main = do
    BS.writeFile "snake.obj" $ asmData block
  where
    block = org 0x50_00 do
        clearScreen
        _ <- ret
        pure ()

videoStart :: Word16
videoStart = 0xc001

numCols :: Word16
numCols = 40

numRows :: Word16
numRows = 25

loopB :: Z80ASM -> Z80ASM
loopB body = do
    rec loop <- label
        body
        djnz loop
    pure ()

clearScreen :: Z80ASM
clearScreen = do
    ld HL videoStart
    rec loop <- label
        ld [HL] 0x20
        inc HL
        ld A H
        cp 0xc4
        jr NZ loop
    pure ()
