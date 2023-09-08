{-# LANGUAGE RecursiveDo, BlockArguments #-}
module Z80.Utils where

import Z80
import Data.Word
import Data.Bits

skippable :: (Location -> Z80 a) -> Z80 a
skippable body = do
    rec
        r <- body end
        end <- label
    pure r

skipped :: Z80 a -> Z80 a
skipped body = skippable \end -> do
    jp end
    body

dw :: [Word16] -> Z80ASM
dw = db . concatMap toBytes
  where
    toBytes w = let (lo, hi) = wordBytes w in [lo, hi]

resw :: Word16 -> Z80ASM
resw = resb . (* 2)

wordBytes :: Word16 -> (Word8, Word8)
wordBytes w = (lo, hi)
  where
    hi = fromIntegral $ w `shiftR` 8
    lo = fromIntegral w
