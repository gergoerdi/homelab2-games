{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module HL2048.Main (game) where

import HL2048
import HL2048.Input
import HL2048.Draw

import Z80
import Z80.Utils
import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char

game :: Z80ASM
game = mdo
    let locs = MkLocs{..}

    clearScreen locs
    prepareGrid locs

    ld HL tileOffs
    decLoopB 16 do
        ld [HL] 0
        inc HL

    call newTileF
    call drawScreenF
    withLabel \loop -> mdo
        halt

        dispatchInput north south east west
        ld DE [rng]
        call lfsr10F
        ld [rng] DE
        jp loop

        let rotMoveRot k ifMoved = skippable \invalidMove -> do
                let ldBoard to from = do
                        ld DE to
                        ld HL from
                        ld BC 16
                        ldir

                -- Save game board
                ldBoard tileScratch tileValues

                -- Rotate game board
                replicateM_ k do
                    ld IX tileValues
                    ld IY tileValues'
                    call rotateF
                    call applyStateF

                -- Calculate movement
                call calcMoveWF

                -- Restore game board
                ldBoard tileValues tileScratch

                -- This is not a valid move if every tile remains at its place
                ld IX tileSpeeds
                ld C 0
                decLoopB 16 do
                    ld A [IX]
                    inc IX
                    skippable \hasNotMoved -> do
                        cp 0
                        jp Z hasNotMoved
                        inc C
                ld A C
                cp 0
                jp Z invalidMove

                -- Rotate end state
                replicateM_ (4 - k) do
                    ld IX tileValues'
                    ld IY tileScratch
                    call rotateF
                    ldBoard tileValues' tileScratch

                -- Rotate movements
                replicateM_ (4 - k) do
                    ld IX tileSpeeds
                    ld IY tileScratch
                    call rotateF
                    ldBoard tileSpeeds tileScratch

                ifMoved

        north <- labelled do
            rotMoveRot 3 do
                ldVia DE [calcAnimSlot] calcAnimNF
                ld B $ 2 * (tileHeight + 3)
                call animateMoveF
                jp moved
            jp loop

        south <- labelled do
            rotMoveRot 1 do
                ldVia DE [calcAnimSlot] calcAnimSF
                ld B $ 2 * (tileHeight + 3)
                call animateMoveF
                jp moved
            jp loop

        east <- labelled do
            rotMoveRot 2 do
                ldVia DE [calcAnimSlot] calcAnimEF
                ld B $ 2 * (tileWidth + 3)
                call animateMoveF
                jp moved
            jp loop

        west <- labelled do
            rotMoveRot 0 do
                ldVia DE [calcAnimSlot] calcAnimWF
                ld B $ 2 * (tileWidth + 3)
                call animateMoveF
                jp moved
            jp loop

        moved <- labelled do
            call newTileF
            call drawScreenF

        jp loop
    loopForever $ pure ()

    drawTileF <- labelled drawTile
    moveTilesF <- labelled $ moveTiles locs
    drawScreenF <- labelled $ do
        drawScreen locs
        ret

    lfsr10F <- labelled lfsr10

    calcMoveWF <- labelled $ calcMoveW locs

    calcAnimNF <- labelled calcAnimN
    calcAnimSF <- labelled calcAnimS
    calcAnimEF <- labelled calcAnimE
    calcAnimWF <- labelled calcAnimW

    calcAnimF <- labelled do
        jp calcAnimEF
    let calcAnimSlot = calcAnimF + 1

    -- | Pre: `B` is number of frames to run
    animateMoveF <- labelled do
        withLabel \runAnim -> do
            push BC
            call moveTilesF
            halt
            call drawScreenF
            pop BC
            djnz runAnim

        call applyStateF
        ld HL tileOffs
        decLoopB 16 do
            ld [HL] 0
            inc HL
        ret

    applyStateF <- labelled do
        ld DE tileValues
        ld HL tileValues'
        ld BC 16
        ldir
        ret

    newTileF <- labelled do
        ld DE [rng]
        call lfsr10F
        ld [rng] DE

        -- With 1/4 chance, new tile value should be 2
        ld B 1
        skippable \new1 -> do
            srl E
            jp C new1
            srl E
            jp C new1
            inc B

        -- We want DE to be a valid tile index, i.e. in 0..15
        ld D 0
        ld A E
        Z80.and 0xf
        ld E A

        -- Is this space occupied already?
        ld IX tileValues
        add IX DE
        ld A [IX]
        cp 0
        jp NZ newTileF

        ld [IX] B
        ret

    rotateF <- labelled do
        -- Maps the board state in IX
        --
        --   0123
        --   4567
        --   89ab
        --   cdef
        --
        -- to the rotated state in IY
        --
        --   c840
        --   d951
        --   ea62
        --   fb73
        --

        ld DE 15
        add IX DE -- Counting down

        ld DE 12 -- DE: offset in target matrix, starts at 12 (see position of `f` above)
        ld C 4 -- Stride for target matrix

        decLoopB 16 $ skippable \next -> do
            push IY
            pop HL
            add HL DE
            ldVia A [HL] [IX]

            dec IX

            ld A E
            sub C
            ld E A

            -- If new input row, then we need to reset the target pointer
            jp NC next
            add A 17
            ld E A

        ret

    rng <- labelled $ dw [1]

    tileValues <- labelled $ db $ replicate 16 0
    tileValues' <- labelled $ resb 16
    tileSpeeds <- labelled $ resb 16
    tileOffs <- labelled $ resb 16
    tileScratch <- labelled $ resb 16
    screenBuf <- labelled $ resb $ 40 * 25
    doubleBuf <- labelled $ resb $ (4 * (tileWidth + 3)) * (4 * (tileHeight + 3))
    pure ()

moveTiles :: Locations -> Z80ASM
moveTiles locs@MkLocs{..} = mdo
    forM_ [0..3] \i -> forM_ [0..3] \j -> do
        -- Apply speed
        ld HL $ tileOffs + j * 4 + i
        ld A [tileSpeeds + j * 4 + i]
        add A [HL]
        ld [HL] A
    ret

calcMoveW :: Locations -> Z80ASM
calcMoveW MkLocs{..} = mdo
    ld IY tileValues'
    ld IX tileSpeeds

    decLoopB 16 do
        ld [IY] 0
        ld [IX] 0
        inc IY
        inc IX

    ld IX tileSpeeds
    ld HL tileValues
    ld DE 0

    forM_ [0..3] \i -> do
        ld E (4 * i)
        ld C 0

        ldVia A [offset] 0
        decLoopB 4 mdo
            ld A [HL]
            cp 0
            jp Z emptyTile

            skippable \end -> mdo
                cp C
                jp Z merge
                ld C A
                jp end

                merge <- label
                inc A
                ld C 0
                dec E

                -- inc [offset]
                push AF
                ld A [offset]
                inc A
                ld [offset] A
                pop AF

            ld IY tileValues'
            add IY DE
            ld [IY] A

            ldVia A [IX] [offset]

            inc E
            jp next

            emptyTile <- labelled do
                -- inc [offset]
                ld A [offset]
                inc A
                ld [offset] A

            next <- label
            inc HL
            inc IX
    ret

    offset <- labelled $ db [0]
    pure ()

-- | An 10-bit maximal LFSR
-- | Pre: `DE` is the current state
-- | Post: `DE` is the new state
-- | Clobbers: `A`
lfsr10 :: Z80ASM
lfsr10 = do
    srl D
    ld A E
    rra
    ld E A
    skippable \skip -> do
        jp NC skip
        Z80.xor 0x04
        ld E A
        ld A 0x02
        Z80.xor D
        ld D A
    ret
