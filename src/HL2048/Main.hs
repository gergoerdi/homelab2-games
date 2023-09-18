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
            decLoopB (tileHeight + 3) do
                push BC
                call moveTilesF
                halt
                call drawScreenF
                pop BC
            jp loop

        south <- labelled do
            -- Demo stuff
            ld DE tileSpeeds
            ld HL tileSpeedsS
            ld BC 16
            ldir

            ldVia DE [calcMoveSlot] calcMoveSF
            decLoopB (tileHeight + 3) do
                push BC
                call moveTilesF
                halt
                call drawScreenF
                pop BC
            jp loop

        east <- labelled do
            -- Demo stuff
            ld DE tileSpeeds
            ld HL tileSpeedsE
            ld BC 16
            ldir

            ldVia DE [calcMoveSlot] calcMoveEF
            decLoopB (tileWidth + 3) do
                push BC
                call moveTilesF
                halt
                call drawScreenF
                pop BC
            jp loop

        west <- labelled do
            -- Demo stuff
            ld DE tileSpeeds
            ld HL tileSpeedsW
            ld BC 16
            ldir

            ldVia DE [calcMoveSlot] calcMoveWF
            decLoopB (tileWidth + 3) do
                push BC
                call moveTilesF
                halt
                call drawScreenF
                pop BC
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

    tileValues <- labelled $ db
      [ 12, 0, 0, 0
      , 1, 0, 1, 0
      , 1, 0, 0, 1
      , 1, 2, 0, 0
      ]

    -- Test: right move
    tileSpeedsE <- labelled $ db
      [ 1, 0, 0, 0
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
