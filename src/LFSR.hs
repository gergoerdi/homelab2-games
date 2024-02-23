module LFSR (lfsr10, lfsr11) where

import Z80
import Z80.Utils
import Data.Word

-- | Pre: `DE` is the current state
-- | Post: `DE` is the new state
-- | Clobbers: `A`
lfsr :: Word16 -> Z80ASM
lfsr coeffs = do
    srl D
    ld A E
    rra
    ld E A
    ret NC
    Z80.xor lo
    ld E A
    ld A hi
    Z80.xor D
    ld D A
    ret
  where
    (lo, hi) = wordBytes coeffs

-- | An 10-bit maximal LFSR
-- | Pre: `DE` is the current state
-- | Post: `DE` is the new state
-- | Clobbers: `A`
lfsr10 :: Z80ASM
lfsr10 = lfsr 0x0204

-- | An 11-bit maximal LFSR
-- | Pre: `DE` is the current state
-- | Post: `DE` is the new state
-- | Clobbers: `A`
lfsr11 :: Z80ASM
lfsr11 = lfsr 0x0402
