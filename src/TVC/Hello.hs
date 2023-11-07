{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecursiveDo #-}
module TVC.Hello (hello) where

import Z80
import Z80.Utils
import TVC
import Control.Monad

hello :: Z80ASM
hello = mdo
    di
    call int1

    loopForever $ return ()

    let setupLineInt y = do
            let (lo, hi) = wordBytes $ (y `div` 4) * 64 {-+ 63-} -- - 46
            crtcOut 0x0e hi
            crtcOut 0x0f lo

    int1 <- labelled mdo
        push AF
        push HL
        out [0x07] A

        -- Set border color to red
        ld A 0b10_00_10_00
        out [0x00] A

        setInterruptHandler int2
        setupLineInt 200

        pop HL
        pop AF
        ei
        ret

    int2 <- labelled do
        push AF
        push HL
        out [0x07] A

        -- Set border color to green
        ld A 0b10_10_00_00
        out [0x00] A

        setInterruptHandler int1
        setupLineInt 160

        pop HL
        pop AF
        ei
        ret

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

    -- -- Set interrupt

    call pageVideoIn
    ld DE videoStart
    decLoopB 8 do
        push BC
        decLoopB 256 do
            push BC
            ld HL colors
            ld BC 8
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
      , 0b0000_0000
      , 0b0000_0000
      , 0b0000_0000
      , 0b0000_0000
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