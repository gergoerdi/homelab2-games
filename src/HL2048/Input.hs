module HL2048.Input where

import HL2048
import Z80
import Z80.Utils

dispatchInput :: Location -> Location -> Location -> Location -> Z80ASM
dispatchInput north south east west = do
    ld A [0x3adf]
    ld C A

    -- Check "i"
    rra
    rra
    jp NC north
    -- Check "j"
    rra
    jp NC west
    -- Check "k"
    rra
    jp NC south
    -- Check "l"
    rra
    jp NC east
