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

    withLabel \loop -> mdo
        halt
        call drawScreenF

        ld HL tileOffs
        decLoopB 16 do
            ld [HL] 0
            inc HL

        dispatchInput north south east west
        jp loop

        north <- labelled do
            -- Demo stuff
            ld DE tileSpeeds
            ld HL tileSpeedsN
            ld BC 16
            ldir

            ldVia DE [calcMoveSlot] calcMoveNF
            ld B $ tileHeight + 3
            call animateMoveF
            jp loop

        south <- labelled do
            -- Demo stuff
            ld DE tileSpeeds
            ld HL tileSpeedsS
            ld BC 16
            ldir

            ldVia DE [calcMoveSlot] calcMoveSF
            ld B $ tileHeight + 3
            call animateMoveF
            jp loop

        east <- labelled do
            -- Demo stuff
            ld DE tileSpeeds
            ld HL tileSpeedsE
            ld BC 16
            ldir

            ldVia DE [calcMoveSlot] calcMoveEF
            ld B $ tileWidth + 3
            call animateMoveF
            jp loop

        west <- labelled do
            -- Demo stuff
            ld DE tileSpeeds
            ld HL tileSpeedsW
            ld BC 16
            ldir

            ldVia DE [calcMoveSlot] calcMoveWF
            ld B $ tileWidth + 3
            call animateMoveF
            jp loop

        pure ()


        -- ld B $ tileHeight + 3
        -- withLabel \loop -> do
        --     push BC
        --     call drawScreenF
        --     call moveTilesF
        --     halt
        --     pop BC
        --     dec B
        --     jp NZ loop

        -- loopForever do
        --     call drawScreenF
        --     halt

    loopForever $ pure ()

    drawTileF <- labelled drawTile
    moveTilesF <- labelled $ moveTiles locs
    drawScreenF <- labelled $ drawScreen locs

    calcMoveNF <- labelled calcMoveN
    calcMoveSF <- labelled calcMoveS
    calcMoveEF <- labelled calcMoveE
    calcMoveWF <- labelled calcMoveW

    calcMoveF <- labelled do
        jp calcMoveEF
    let calcMoveSlot = calcMoveF + 1

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
        ret

    applyStateF <- labelled do
        ld DE tileValues
        ld HL tileValues'
        ld BC 16
        ldir
        ret


    rotateF <- labelled do
        -- Maps the board state in tileValues
        --
        --   0123
        --   4567
        --   89ab
        --   cdef
        --
        -- to the rotated state in tileValues'
        --
        --   c840
        --   d951
        --   ea62
        --   fb73
        --
        ld IX $ tileValues + 15

        ld D 0 -- DE: offset in target matrix, starts at 12 (see position of `f` above)
        ld E 12
        ld C 4 -- Stride for target matrix

        decLoopB 16 $ skippable \next -> do
            ld IY tileValues'
            add IY DE
            ldVia A [IY] [IX]

            dec IX

            ld A E
            sub C
            ld E A

            -- If new input row, then we need to reset the target pointer
            jp NC next
            add A 17
            ld E A

        ret

    tileValues <- labelled $ db
      -- [ 12, 0, 0, 0
      -- , 1, 0, 1, 0
      -- , 1, 0, 0, 1
      -- , 1, 2, 0, 0
      -- ]
      [ 1..16 ]

    tileValues' <- labelled $ db
      [ 0, 0, 0, 12
      , 0, 0, 0, 2
      , 0, 0, 0, 2
      , 0, 0, 1, 2
      ]

    -- Test: right move
    tileSpeedsE <- labelled $ db
      [ 3, 0, 0, 0
      , 3, 0, 1, 0
      , 3, 0, 0, 0
      , 2, 2, 0, 0
      ]

    -- Test: left move
    tileSpeedsW <- labelled $ db
      [ 0, 0, 0, 0
      , 0, 0, 2, 0
      , 0, 0, 0, 3
      , 0, 1, 0, 0
      ]

    -- Test: down move
    tileSpeedsS <- labelled $ db
      [ 1, 0, 0, 0
      , 1, 0, 2, 0
      , 1, 0, 0, 1
      , 0, 0, 0, 0
      ]

    -- Test: up move
    tileSpeedsN <- labelled $ db
      [ 0, 0, 0, 0
      , 0, 0, 1, 0
      , 1, 0, 0, 2
      , 1, 3, 0, 0
      ]

    tileSpeeds <- labelled $ resb 16
    tileOffs <- labelled $ resb 16
    screenBuf <- labelled $ resb $ 40 * 25
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
