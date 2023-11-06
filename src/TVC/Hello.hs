{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecursiveDo #-}
module TVC.Hello (hello) where

import Z80
import TVC
import Control.Monad

-- https://github.com/konzolcowboy/TVCStudio/blob/master/Peldak/pr13.tvcasm
hello :: Z80ASM
hello = mdo
    -- Save current graphics settings
    ld A [0x0b13]
    push AF

    Z80.and 0b1111_1100
    Z80.or  0b0000_0010 -- Graphics mode 16
    -- Z80.or  0b0000_0001 -- Graphics mode 4
    -- Z80.or  0b0000_0000 -- Graphics mode 2
    out [0x06] A

    -- Clear screen
    syscall 0x05

    call pageVideoIn
    ld DE videoStart
    decLoopB 16 do
        push BC
        decLoopB 256 do
            push BC
            ld HL colors
            ld BC 4
            ldir
            pop BC
        pop BC
    call pageVideoOut

    -- Wait for keypress
    syscall 0x91

    -- Resore video mode
    pop AF
    out [0x06] A

    ret

    colors <- labelled $ db
      [ 0b1101_1000 -- RG
      , 0b1100_0010 -- BK
      , 0b1111_1101
      , 0b1101_1011
      ]

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
