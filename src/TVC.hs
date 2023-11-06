module TVC where

import Z80
import Data.Word

videoStart :: Word16
videoStart = 0x8000

syscall :: Word8 -> Z80ASM
syscall op = do
    rst 0x30
    db [op]
