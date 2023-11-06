{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecursiveDo #-}
module TVC.Hello (hello) where

import Z80
import TVC

-- https://github.com/konzolcowboy/TVCStudio/blob/master/Peldak/pr13.tvcasm
hello :: Z80ASM
hello = mdo
    -- Save current graphics settings
    ld A [0x0b13]
    push AF

    -- Graphics mode 16
    Z80.and 0b1111_1100
    Z80.or  0b0000_0010
    out [0x06] A

    -- Clear screen
    syscall 0x05

    call pageVideoIn
    ld DE videoStart
    ld HL colors
    ld BC 0x08
    ldir
    call pageVideoOut

    -- Wait for keypress
    syscall 0x91

    -- Resore video mode
    pop AF
    out [0x06] A
    ret

    colors <- labelled $ db [ 0x01, 0x0d, 0x31, 0x3d, 0xc1, 0xcd, 0xf1, 0xfd ]

    pageVideoIn <- labelled do
        ld A 0x50
        ld [0x03] A
        out [0x02] A
        ret

    pageVideoOut <- labelled do
        ld A 0x70
        ld [0x03] A
        out [0x02] A
        ret

    pure ()
