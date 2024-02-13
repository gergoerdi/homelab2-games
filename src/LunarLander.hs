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
    drawDown <- labelled $ db [0]
    drawLeft <- labelled $ db [0]
    drawRight <- labelled $ db [0]

    let platformWidth :: Num a => a
        platformWidth = 6
        platformCols :: Integral a => a
        platformCols = rowstride `div` platformWidth

    terrain <- labelled $ db $ replicate rowstride 0
    platform <- labelled $ db [0]

    lfsr <- labelled lfsr10

    initTerrain <- labelled do
        ld DE 0x1234 -- TODO: persist this between runs

        -- Choose a random column for the platform
        withLabel \loop -> do
            call lfsr
            ld A E
            Z80.and 0x3f
            cp (rowstride - platformWidth)
            jp NC loop
        ld [platform] A
        ld C A

        ld HL terrain
        decLoopB (rowstride - platformWidth + 1) do
            call lfsr
            ld A E
            Z80.and 0x07
            Z80.or 0x18
            ld [HL] A
            inc HL

            -- Is this where the platform is?
            push AF
            ld A C
            cp B
            unlessFlag NZ do
                pop AF
                ld C B
                decLoopB (platformWidth - 1) do
                    ld [HL] A
                    inc HL
                ld B C
                ld C 0xff -- Ensure no more trigger of this branch
                push AF
            pop AF
        ret

    drawTerrain <- labelled do
        ld IY terrain
        ld IX videoStart

        -- We'll use this to randomize the surface "texture"
        ld DE 0xffff
        push DE

        decLoopB rowstride do
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

            -- Make the surface a bit ragged
            pop DE
            call lfsr
            ld A E
            Z80.and 0x03
            Z80.or 0xfc
            ld [HL] A
            push DE

            ld DE rowstride
            add HL DE

            push BC
            withLabel \loop -> do
                ld A 0xff
                ld [HL] A
                add HL DE
                ld A H
                cp 0x00
                jp NZ loop
            pop BC

        pop DE
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
        ld A 0
        ld [drawDown] A
        ld [drawLeft] A
        ld [drawRight] A

        ld A [0xe800]

        let checkKey body = skippable \notThis -> do
                rra
                jp C notThis
                body
            setDir dir = do
                ld IX dir
                dec [IX]

        ld HL [landerVY]
        checkKey do -- Down
            ld DE $ negate 0x00_04
            add HL DE
            ld [landerVY] HL
            setDir drawDown

        checkKey do -- Up
            pure ()

        ld HL [landerVX]
        checkKey do -- Right
            ld DE $ negate 0x00_02
            add HL DE
            ld [landerVX] HL
            setDir drawRight

        checkKey do -- Left
            ld DE 0x00_02
            add HL DE
            ld [landerVX] HL
            setDir drawLeft

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

    landerSprite <- labelled $ db $ concat landerSprite_
    downSprite1 <- labelled $ db $ concat downSprite1_
    downSprite2 <- labelled $ db $ concat downSprite2_
    rightSprite1 <- labelled $ db $ concat rightSprite1_
    rightSprite2 <- labelled $ db $ concat rightSprite2_
    leftSprite1 <- labelled $ db $ concat leftSprite1_
    leftSprite2 <- labelled $ db $ concat leftSprite2_

    drawLander <- labelled do
        ld IX landerSprite
        call drawSprite

        let drawSprites sprite1 sprite2 = do
                ld IX frame
                Z80.bit 0 [IX]
                ld IX sprite1
                unlessFlag Z $ ld IX sprite2
                call drawSprite

        let drawSpritesIf dir sprite1 sprite2 = do
                ld A [dir]
                Z80.and A
                unlessFlag Z $ drawSprites sprite1 sprite2

        drawSpritesIf drawDown downSprite1 downSprite2
        drawSpritesIf drawLeft leftSprite1 leftSprite2
        drawSpritesIf drawRight rightSprite1 rightSprite2

        ret

    -- | Pre: `IX` contains sprite's starting address
    drawSprite <- labelled do
        -- ld IX landerPic
        -- ld L [IX]
        -- ld H [IX + 1]
        -- push HL
        -- pop IX

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

landerSprite_ :: [[Word8]]
landerSprite_ =
    [ [ 0x00, 0x1a, 0x12, 0x1e, 0x00 ]
    , [ 0x00, 0xea, 0x14, 0xd5, 0x00 ]
    , [ 0x00, 0x14, 0x14, 0x14, 0x00 ]
    , [ 0x18, 0x1e, 0x00, 0x1d, 0x15 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    ]

downSprite1_ :: [[Word8]]
downSprite1_ =
    [ [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0xd9, 0x00, 0x00 ]
    , [ 0x00, 0xe6, 0xe6, 0xe6, 0x00 ]
    ]

downSprite2_ :: [[Word8]]
downSprite2_ =
    [ [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0xd5, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0xe6, 0x00, 0x00 ]
    , [ 0x00, 0xd9, 0xd9, 0xd9, 0x00 ]
    ]

rightSprite1_ :: [[Word8]]
rightSprite1_ =
    [ [ 0x00, 0x00, 0x00, 0x00, 0x1e ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x19 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    ]

rightSprite2_ :: [[Word8]]
rightSprite2_ =
    [ [ 0x00, 0x00, 0x00, 0x00, 0x1d ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x1a ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    ]

leftSprite1_ :: [[Word8]]
leftSprite1_ =
    [ [ 0x1d, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x1a, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    ]

leftSprite2_ :: [[Word8]]
leftSprite2_ =
    [ [ 0x1e, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x19, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
    ]
