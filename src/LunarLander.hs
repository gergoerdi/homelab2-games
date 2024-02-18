module LunarLander where

import Z80
import Z80.Utils
import Z80.Machine.HomeLab.HL34
import Data.Word
import Data.Bits
import Control.Monad (forM_, replicateM_)
import LFSR

game :: Z80ASM
game = mdo
    di
    pageIO

    ldVia DE [terrainRNG] 0x0123

    loopForever do -- New level
        call initTerrain

        skippable \nextLevel -> loopForever do -- Play level
            ldVia DE [landerX] (32 `shiftL` 10)
            ldVia DE [landerVX] 0x0000
            ldVia DE [landerY] (1 `shiftL` 11)
            ldVia DE [landerVY] 0x0000

            call clearScreen
            call drawTerrain

            skippable \endGame -> loopForever do
                ld HL frame
                inc [HL]

                call clearLander
                call drawTerrainTop

                -- Apply gravity
                ld HL [landerVY]
                ld DE 0x00_01
                add HL DE
                ld [landerVY] HL

                call scanKeys
                call moveLander
                unlessFlag Z do
                    call checkSpeed
                    jp NZ endGame
                    jp nextLevel

                call drawLander

                jp NZ endGame

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
    landerX <- labelled $ dw [30 `shiftL` 10]
    landerY <- labelled $ dw [4 * 256]
    landerVX <- labelled $ dw [0]
    landerVY <- labelled $ dw [0]
    drawDown <- labelled $ db [0]
    drawLeft <- labelled $ db [0]
    drawRight <- labelled $ db [0]

    let platformWidth :: Num a => a
        platformWidth = 8
        landerWidth :: Num a => a
        landerWidth = 5

    terrain <- labelled $ db $ replicate rowstride 0
    platform <- labelled $ db [0]
    terrainRNG <- labelled $ dw [0]

    lfsr <- labelled lfsr10

    initTerrain <- labelled do
        ld DE [terrainRNG]

        -- Choose a random column for the platform
        withLabel \loop -> do
            call lfsr
            ld A E
            Z80.and 0x3f
            cp 0
            jp Z loop
            cp (rowstride - platformWidth + 2)
            jp NC loop
        ld [platform] A
        ld C A

        ld HL terrain

        call lfsr
        ld A E
        Z80.and 0x07
        add A 23

        decLoopB (rowstride - platformWidth + 1) do
            -- Calculate into A the next height
            push BC
            ld B A
            call lfsr
            ld A E
            Z80.and 0x07
            sub 4
            add A B
            pop BC


            cp 31
            unlessFlag C $ ld A 29
            cp 8
            unlessFlag NC $ ld A 10

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
        ld [terrainRNG] DE
        ret

    -- | Post: `Z` flag is set if our speed is good for landing
    checkSpeed <- labelled do
        ld BC [landerVX]
        ld DE [landerVY]

        -- Check high bytes first
        ld A B
        Z80.or A
        ret NZ
        ld A D
        Z80.or A
        ret NZ

        -- Check low bytes
        Z80.and 0b1100_0000
        ret NZ
        ld A B
        Z80.and 0b1100_0000
        ret

    drawTerrain <- labelled do
        ld IY terrain
        ld IX videoStart

        -- We'll use this to randomize the surface "texture"
        ld DE 0xffff
        push DE

        decLoopB rowstride mdo
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

            -- Is this the platform?
            ld A [platform]
            add A (platformWidth - 1)
            sub B
            cp platformWidth
            jp C drawPlatform

            -- If not, make the surface a bit ragged so the platform stands out
            pop DE
            call lfsr
            ld A E
            Z80.and 0x02
            Z80.add A 0xfc
            push DE
            jp afterPlatform

            drawPlatform <- labelled do
                ld A 0xa2

            afterPlatform <- label
            ld [HL] A

            ld DE rowstride
            add HL DE

            -- Draw the rest of the lines all the way to bottom of screen
            push BC
            withLabel \loop -> do
                ld A 0xff
                ld [HL] A
                add HL DE
                ld A H
                cp 0x00 -- We've reached the end of the screen when we've wrapped around the memory address
                jp NZ loop
            pop BC

        pop DE
        ret

    drawTerrainTop <- labelled do
        ld IY terrain
        ld IX videoStart

        -- We'll use this to randomize the surface "texture"
        ld DE 0xffff
        push DE

        decLoopB rowstride mdo
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

            -- Is this the platform?
            ld A [platform]
            add A (platformWidth - 1)
            sub B
            cp platformWidth
            jp C drawPlatform

            -- If not, make the surface a bit ragged so the platform stands out
            pop DE
            call lfsr
            ld A E
            Z80.and 0x02
            Z80.add A 0xfc
            push DE
            jp afterPlatform

            drawPlatform <- labelled do
                ld A 0xa2

            afterPlatform <- label
            ld [HL] A

            ld DE rowstride
            add HL DE

        pop DE
        ret

    -- | Post: `Z` flag is cleared iff we have landed on the platform (regardless of speed)
    moveLander <- labelled do
        ld HL [landerX]
        ld DE [landerVX]
        add HL DE
        ld [landerX] HL

        ld HL [landerY]
        ld DE [landerVY]
        add HL DE

        -- Clamp top
        ld A H
        cp (31 * 8)
        unlessFlag C do
            ld H 0
            ldVia DE [landerVY] 0

        -- Did we just land? `C` will record if yes.
        ld C 0

        skippable \tooHigh -> do
            ld IX terrain
            ld D 0
            ldVia A E [landerX  + 1]
            replicateM_ 2 $ srl E
            add IX DE
            ld A [IX]
            sub 4

            replicateM_ 3 $ sla A
            cp H
            jp NC tooHigh

            ld A [landerX + 1]
            replicateM_ 2 $ srl A
            neg
            add A 64
            ld E A

            ld A [platform]
            add A (platformWidth - 1)
            sub E
            cp (platformWidth - landerWidth + 1)
            unlessFlag NC $ dec C

        ld [landerY] HL
        ld A C
        Z80.or A
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
        replicateM_ 2 $ srl E
        add HL DE

        -- Apply Y coordinate
        ldVia A E [landerY + 1]
        replicateM_ 3 $ srl E
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

        ld IX landerSprite
        jp drawSprite

    -- | Pre: `IX` contains sprite's starting address
    -- | Post: `Z` flag is cleared iff there's been a collision
    drawSprite <- labelled mdo
        ldVia A [collision] 0

        call landerScreenAddr
        decLoopB 5 do
            ld C B

            ld DE $ rowstride - 5
            decLoopB landerWidth do
                push DE

                ld A [IX]
                inc IX
                Z80.and A
                unlessFlag Z $ do
                    ld D A
                    ld A [HL]
                    Z80.and A
                    unlessFlag Z $ ldVia A [collision] 1
                    ld [HL] D

                -- Increment HL, then check if it would have caused a line break.
                ld D H
                ld A L
                Z80.and 0b1100_0000
                inc HL
                ld E A
                ld A L
                Z80.and 0b1100_0000

                skippable \noLineBreak -> mdo
                    -- Did we stay on the same line?
                    cp E
                    jp NZ lineBreak

                    -- We did, so it's all OK -- HL is correct, and DE needs no updating
                    pop DE
                    jp noLineBreak

                    -- We didn't, so reset HL to start of old HL's line (temporarily stored in DE)
                    lineBreak <- label
                    ex DE HL
                    pop DE
                    ld DE $ rowstride + rowstride - landerWidth


            add HL DE
            ld B C

        ld A [collision]
        Z80.and A
        ret
        collision <- labelled $ db [0]
        pure ()

    clearLander <- labelled do
        call landerScreenAddr
        ld DE $ rowstride - landerWidth
        ld A 0x00
        decLoopB 5 do
            ld C B
            decLoopB landerWidth do
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
    , [ 0x00, 0x00, 0x00, 0x00, 0x00 ]
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
