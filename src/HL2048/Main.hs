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

    loopForever do
        ld B $ tileHeight + 3
        withLabel \loop -> do
            push BC
            call drawScreenF
            call moveTilesF
            halt
            pop BC
            dec B
            jp NZ loop

        loopForever do
            call drawScreenF
            halt

    loopForever $ pure ()

    drawTileF <- labelled drawTile
    moveTilesF <- labelled $ moveTiles locs
    drawScreenF <- labelled $ drawScreen locs
    anim <- labelled $ db [0]

    tileValues <- labelled $ db
      [ 12, 0, 0, 0
      , 1, 0, 1, 0
      , 1, 0, 0, 1
      , 1, 2, 0, 0
      ]

    -- -- Test: right move
    -- tileSpeeds <- labelled $ db
    --   [ 1, 0, 0, 0
    --   , 3, 0, 1, 0
    --   , 3, 0, 0, 0
    --   , 2, 2, 0, 0
    --   ]

    -- -- Test: left move
    -- tileSpeeds <- labelled $ db
    --   [ 0, 0, 0, 0
    --   , 0, 0, 2, 0
    --   , 0, 0, 0, 3
    --   , 0, 1, 0, 0
    --   ]

    -- -- Test: down move
    -- tileSpeeds <- labelled $ db
    --   [ 1, 0, 0, 0
    --   , 1, 0, 2, 0
    --   , 1, 0, 0, 1
    --   , 0, 0, 0, 0
    --   ]

    -- Test: up move
    tileSpeeds <- labelled $ db
      [ 0, 0, 0, 0
      , 0, 0, 1, 0
      , 1, 0, 0, 2
      , 1, 3, 0, 0
      ]

    tileOffs <- labelled $ db
      [ 0, 0, 0, 0
      , 0, 0, 0, 0
      , 0, 0, 0, 0
      , 0, 0, 0, 0
      ]

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
