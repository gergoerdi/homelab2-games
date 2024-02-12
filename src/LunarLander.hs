module LunarLander where

import Z80
import Z80.Utils
import Z80.Machine.HomeLab.HL34
import Data.Word
import Control.Monad (forM_)

game :: Z80ASM
game = mdo
    di
    pageIO

    call clearScreen
    call drawLander
    loopForever $ pure ()

    clearScreen <- labelled clearScreen_
    landerX <- labelled $ dw [30]
    landerY <- labelled $ dw [1]

    landerPic <- labelled $ db $ concat lander

    drawLander <- labelled do
        ld D 0
        ldVia A E [landerX]
        ld HL videoStart
        add HL DE
        ldVia A E [landerY]
        decLoopB 6 do
            sla E
            unlessFlag NC $ rl D
        add HL DE

        ld DE $ rowstride - 5
        ld IX landerPic
        decLoopB 4 do
            ld C B
            decLoopB 5 do
                ldVia A [HL] [IX]
                inc IX
                inc HL
            add HL DE
            ld B C
        ret
    pure ()

clearScreen_ :: Z80ASM
clearScreen_ = do
    ld DE videoStart
    ld A 0x00
    decLoopB 8 do
        ld C B
        decLoopB 256 do
            ld [DE] A
            inc DE
        ld B C
    ret

lander :: [[Word8]]
lander =
    [ [ 0x00, 0x1a, 0x12, 0x1e, 0x00 ]
    , [ 0x00, 0xea, 0x14, 0xd5, 0x00 ]
    , [ 0x00, 0x14, 0x14, 0x14, 0x00 ]
    , [ 0x18, 0x1e, 0x00, 0x1d, 0x15 ]
    ]
