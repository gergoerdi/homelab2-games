{-# LANGUAGE NumericUnderscores, BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Z80
import qualified Data.ByteString as BS
import Text.Printf

main :: IO ()
main = do
    mapM_ (printf "%02x\n") $ BS.unpack $ asmData block
  where
    block = org 0x50_00 do
        ld A 0xff
