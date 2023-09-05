{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Tetris.Main
import Snake.Main

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
        -- tetris
        snake
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
