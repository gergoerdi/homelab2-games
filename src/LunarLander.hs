module LunarLander where

import Z80
import Z80.Utils
import Z80.Machine.HomeLab.HL34
import Data.Word
import Control.Monad (forM_, replicateM_)
import LFSR

game :: Z80ASM
game = mdo
    di
    pageIO

    let setPic pic = do
            ld IX landerPic
            ld HL pic
            ld [IX] L
            ld [IX + 1] H

    loopForever do
        call clearScreen
        call initTerrain
        call drawTerrain

        loopForever do
            ld HL frame
            inc [HL]

            call clearLander
            call drawTerrain

            -- Apply gravity
            ld HL [landerVY]
            ld DE 0x00_01
            add HL DE
            ld [landerVY] HL

            setPic landerPicIdle
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
    frame <- labelled $ db [0]
    landerX <- labelled $ dw [30 * 256]
    landerY <- labelled $ dw [4 * 256]
    landerVX <- labelled $ dw [0]
    landerVY <- labelled $ dw [0]

    landerPicIdle <- labelled $ db $ concat landerIdle
    landerPicDown1 <- labelled $ db $ concat landerDown1
    landerPicDown2 <- labelled $ db $ concat landerDown2
    landerPicRight1 <- labelled $ db $ concat landerRight1
    landerPicRight2 <- labelled $ db $ concat landerRight2
    landerPicLeft1 <-  labelled $ db $ concat landerLeft1
    landerPicLeft2 <- labelled $ db $ concat landerLeft2
    landerPic <- labelled $ dw [landerPicIdle]

    let platformWidth :: Num a => a
        platformWidth = 6
        platformCols :: Integral a => a
        platformCols = rowstride `div` platformWidth

    terrain <- labelled $ db $ replicate rowstride 0
    platform <- labelled $ db [0]

    lfsr <- labelled lfsr10

    initTerrain <- labelled do
        ld DE 0x01 -- TODO: persist this between runs
        ld HL terrain
        decLoopB platformCols do
            call lfsr
            ld A E
            Z80.and 0x07
            Z80.or 0x18
            ld C B
            decLoopB platformWidth do
                ld [HL] A
                inc HL
            ld B C
        ret

    drawTerrain <- labelled do
        ld IY terrain
        ld IX $ videoStart + ((rowstride `mod` platformWidth) `div` 2)
        decLoopB (platformCols * platformWidth) do
            push IX
            pop HL
            inc IX

            -- Calculate into HL the start (topmost pixel) of the given terrain
            ld D 0
            ldVia A E [IY]
            inc IY
            replicateM_ 6 do
                sla E
                rl D
            add HL DE

            ld DE rowstride
            push BC
            withLabel \loop -> do
                ld A 0xff
                ld [HL] A
                add HL DE
                ld A H
                cp 0x00
                jp NZ loop
            pop BC
        ret

    moveLander <- labelled do
        ld HL [landerX]
        ld DE [landerVX]
        add HL DE

        -- Clamp left-hand side
        ld A H
        cp 128
        unlessFlag C do
            ld H 0
            -- ld DE 0
            -- ld [landerVX] DE

        -- Clamp right-hand side
        ld A H
        cp (128 - 5 * 2)
        unlessFlag C do
            ld H (128 - 5 * 2)
            -- ld DE 0
            -- ld [landerVX] DE
        ld [landerX] HL

        ld HL [landerY]
        ld DE [landerVY]
        add HL DE

        -- Clamp top
        ld A H
        cp 180
        unlessFlag C do
            ld H 0
            -- ld DE 0
            -- ld [landerVY] DE

        -- Clamp bottom
        -- ld IX terrain
        -- ld D 0
        -- ldVia A E [landerX  + 1]
        -- replicateM_ 2 $
        sra E
        -- add IX DE
        -- ld A [IX]
        -- sub 4
        ld A 80
        cp H
        unlessFlag NC do
            ld H A

        ld [landerY] HL
        ret

    scanKeys <- labelled do
        ld A [0xe800]

        let checkKey body = skippable \notThis -> do
                rra
                jp C notThis
                body
                ret

        let setPic2 pic1 pic2 = do
                skippable \end -> do
                    ld B A
                    ld A [frame]
                    Z80.and 0x01
                    unlessFlag Z do
                        setPic pic1
                        jp end
                    setPic pic2
                ld A B

        ld HL [landerVY]
        checkKey do -- Down
            ld DE $ negate 0x00_04
            add HL DE
            ld [landerVY] HL
            setPic2 landerPicDown1 landerPicDown2

        checkKey do -- Up
            pure ()

        ld HL [landerVX]
        checkKey do -- Right
            ld DE $ negate 0x00_02
            add HL DE
            ld [landerVX] HL
            setPic2 landerPicRight1 landerPicRight2

        checkKey do -- Left
            ld DE 0x00_02
            add HL DE
            ld [landerVX] HL
            setPic2 landerPicLeft1 landerPicLeft2

        ret

    -- | Post: `HL` points to screen address where lander should be drawn
    landerScreenAddr <- labelled do
        ld HL videoStart
        ld D 0

        -- Apply X coordinate
        ldVia A E [landerX + 1]
        replicateM_ 1 $ sra E
        add HL DE

        -- Apply Y coordinate
        ldVia A E [landerY + 1]
        replicateM_ 2 $ sra E
        decLoopB 6 do
            sla E
            rl D
        add HL DE

        ret

    drawLander <- labelled do
        ld IX landerPic
        ld L [IX]
        ld H [IX + 1]
        push HL
        pop IX

        call landerScreenAddr
        ld DE $ rowstride - 5
        decLoopB 5 do
            ld C B
            decLoopB 5 do
                ld A [IX]
                inc IX
                Z80.and A
                unlessFlag Z $ ld [HL] A
                inc HL
            add HL DE
            ld B C
        ret

    clearLander <- labelled do
        call landerScreenAddr
        ld DE $ rowstride - 5
        ld A 0x00
        decLoopB 5 do
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

landerIdle :: [[Word8]]
landerIdle =
    [ [ 0x00, 0x1a, 0x12, 0x1e, 0x00 ]
    , [ 0x00, 0xea, 0x14, 0xd5, 0x00 ]
    , [ 0x00, 0x14, 0x14, 0x14, 0x00 ]
    , [ 0x18, 0x1e, 0x00, 0x1d, 0x15 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    ]

landerDown1 :: [[Word8]]
landerDown1 =
    [ [ 0x00, 0x1a, 0x12, 0x1e, 0x00 ]
    , [ 0x00, 0xea, 0x14, 0xd5, 0x00 ]
    , [ 0x00, 0x14, 0x14, 0x14, 0x00 ]
    , [ 0x18, 0x1e, 0xd9, 0x1d, 0x15 ]
    , [ 0x00, 0xe6, 0xe6, 0xe6, 0x00 ]
    ]

landerDown2 :: [[Word8]]
landerDown2 =
    [ [ 0x00, 0x1a, 0x12, 0x1e, 0x00 ]
    , [ 0x00, 0xea, 0x14, 0xd5, 0x00 ]
    , [ 0x00, 0x14, 0x14, 0x14, 0x00 ]
    , [ 0x18, 0x1e, 0xe6, 0x1d, 0x15 ]
    , [ 0x00, 0xd9, 0xd9, 0xd9, 0x00 ]
    ]

landerRight1 :: [[Word8]]
landerRight1 =
    [ [ 0x00, 0x1a, 0x12, 0x1e, 0x1e ]
    , [ 0x00, 0xea, 0x14, 0xd5, 0x19 ]
    , [ 0x00, 0x14, 0x14, 0x14, 0x00 ]
    , [ 0x18, 0x1e, 0x00, 0x1d, 0x15 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    ]

landerRight2 :: [[Word8]]
landerRight2 =
    [ [ 0x00, 0x1a, 0x12, 0x1e, 0x1d ]
    , [ 0x00, 0xea, 0x14, 0xd5, 0x1a ]
    , [ 0x00, 0x14, 0x14, 0x14, 0x00 ]
    , [ 0x18, 0x1e, 0x00, 0x1d, 0x15 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    ]

landerLeft1 :: [[Word8]]
landerLeft1 =
    [ [ 0x1d, 0x1a, 0x12, 0x1e, 0x00 ]
    , [ 0x1a, 0xea, 0x14, 0xd5, 0x00 ]
    , [ 0x00, 0x14, 0x14, 0x14, 0x00 ]
    , [ 0x18, 0x1e, 0x00, 0x1d, 0x15 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    ]

landerLeft2 :: [[Word8]]
landerLeft2 =
    [ [ 0x1e, 0x1a, 0x12, 0x1e, 0x00 ]
    , [ 0x19, 0xea, 0x14, 0xd5, 0x00 ]
    , [ 0x00, 0x14, 0x14, 0x14, 0x00 ]
    , [ 0x18, 0x1e, 0x00, 0x1d, 0x15 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    ]

landerDownRight1 :: [[Word8]]
landerDownRight1 =
    [ [ 0x00, 0x1a, 0x12, 0x1e, 0x1e ]
    , [ 0x00, 0xea, 0x14, 0xd5, 0x19 ]
    , [ 0x00, 0x14, 0x14, 0x14, 0x00 ]
    , [ 0x18, 0x1e, 0xd9, 0x1d, 0x15 ]
    , [ 0x00, 0xe6, 0xe6, 0xe6, 0x00 ]
    ]

landerDownRight2 :: [[Word8]]
landerDownRight2 =
    [ [ 0x00, 0x1a, 0x12, 0x1e, 0x1d ]
    , [ 0x00, 0xea, 0x14, 0xd5, 0x1a ]
    , [ 0x00, 0x14, 0x14, 0x14, 0x00 ]
    , [ 0x18, 0x1e, 0xe6, 0x1d, 0x15 ]
    , [ 0x00, 0xd9, 0xd9, 0xd9, 0x00 ]
    ]
