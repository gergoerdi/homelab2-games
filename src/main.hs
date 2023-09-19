{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Tetris.Main
import Snake.Main
import qualified HL2048.Main as HL2048

import Z80
import Z80.Utils
import Data.Word
import qualified Data.ByteString as BS
import Control.Monad
import Data.Bits
import Data.Char
import Text.Printf
import Data.String (fromString)

main :: IO ()
main = do
    BS.writeFile "snake.htp" $ htp (fromString "snake") block
  where
    block = org 20000 do
        -- booter
        -- tetris
        -- snake
        HL2048.game

labelASCII :: Location -> [Word8]
labelASCII loc = map (+ 0x30) $ digits
  where
    -- The length of this has to be lazy in the actual value of `loc`
    digits = [fromIntegral $ (loc `div` (10 ^ i)) `mod` 10 | i <- [4, 3 .. 0]]

htp :: BS.ByteString -> ASMBlock -> BS.ByteString
htp label mainBlock = mconcat
    [ leader
    -- , record label $ sysvars $ asmOrg mainBlock
    , record label $ org 0x4002 do
            dw [asmOrg mainBlock]
    , BS.singleton 0x01
    , leader
    , record mempty mainBlock
    , BS.singleton 0x00
    ]
  where
    leader = BS.replicate 100 0x00

    record label block = mconcat
        [ BS.singleton 0xa5
        , label
        , BS.singleton 0x00
        , word $ asmOrg block
        , word . fromIntegral $ BS.length bs
        , bs
        , crc bs
        ]
      where
        bs = asmData block

    crc = BS.singleton . BS.foldr' (+) 0

    word w = BS.pack [lo, hi]
      where
        (lo, hi) = wordBytes w
