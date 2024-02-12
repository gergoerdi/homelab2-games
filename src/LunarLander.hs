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
    loopForever do
        -- Apply gravity every second frame
        ld A [frame]
        Z80.and 0x01
        unlessFlag Z do
            ld HL [landerVY]
            ld DE 0x00_01
            add HL DE
            ld [landerVY] HL
        call scanKeys
        call moveLander
        call drawLander

        -- Wait for end vblank
        withLabel \waitFrame1 -> do
            ld A [0xe802]
            Z80.bit 0 A
            jp NZ waitFrame1

        -- Wait for start of vblank
        withLabel \waitFrame2 -> do
            ld A [0xe802]
            Z80.bit 0 A
            jp Z waitFrame2

    clearScreen <- labelled clearScreen_
    landerX <- labelled $ dw [30 * 256]
    landerY <- labelled $ dw [1 * 256]
    landerVX <- labelled $ dw [0]
    landerVY <- labelled $ dw [0]

    landerPic <- labelled $ db $ concat lander

    moveLander <- labelled do
        ld HL [landerX]
        ld DE [landerVX]
        add HL DE
        ld [landerX] HL

        ld HL [landerY]
        ld DE [landerVY]
        add HL DE
        ld [landerY] HL
        ret

    scanKeys <- labelled do
        ld A [0xe800]

        let checkKey body = skippable \notThis -> do
                rra
                jp C notThis
                body
                ret

        ld HL [landerVY]
        checkKey do -- Down
            ld DE $ negate 0x00_02
            add HL DE
            ld [landerVY] HL

        checkKey do -- Up
            ld DE 0x00_02
            add HL DE
            ld [landerVY] HL

        ld HL [landerVX]
        checkKey do -- Right
            ld DE $ negate 0x00_01
            add HL DE
            ld [landerVX] HL

        checkKey do -- Left
            ld DE 0x00_01
            add HL DE
            ld [landerVX] HL

        ret

    -- | Post: `HL` points to screen address where lander should be drawn
    landerScreenAddr <- labelled do
        ld D 0
        ldVia A E [landerX + 1]
        ld HL videoStart
        add HL DE
        ldVia A E [landerY + 1]
        decLoopB 6 do
            sla E
            rl D
        add HL DE
        ret

    drawLander <- labelled do
        call landerScreenAddr
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

    clearLander <- labelled do
        call landerScreenAddr
        ld DE $ rowstride - 5
        ld A 0x00
        decLoopB 4 do
            ld C B
            decLoopB 5 do
                ld [HL] A
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
