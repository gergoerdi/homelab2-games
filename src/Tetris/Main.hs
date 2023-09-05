{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Tetris.Main (tetris) where

import Tetris
import Tetris.Draw

import Z80
import Data.Word
import Data.Bits
import Control.Monad

tetris :: Z80ASM
tetris = do
    clearScreen
    rec
        drawSkeleton locs

        -- drawTetris locs
        -- loopForever $ pure ()

        loopForever do
            waitFrame
            updateState locs
            drawTetris locs
        pieces <- labelled $ dw allPieces
        dw [0, 0]
        wellContents <- labelled $ dw testState -- $ map (`shiftL` 4) $ replicate wellHeight 0b1_00000_00000_1 ++ [0b1_11111_11111_1]
        currentPiece <- labelled $ db [0]
        currentRot <- labelled $ db [0]
        currentPos <- labelled $ db [4]
        pieceBuf <- labelled $ dw $ take (4 * 2) allPieces
        lineBuf <- labelled $ dw [0]
        fallHeight <- labelled $ db [0]
        delay <- labelled $ db [15]
        levelDelay <- labelled $ db [15]
        let locs = MkLocs{..}

    pure ()

skippable :: (Location -> Z80ASM) -> Z80ASM
skippable body = do
    rec
        body end
        end <- label
    pure ()

updateState :: Locations -> Z80ASM
updateState locs@MkLocs{..} = skippable \done -> do
    rec
        checkCollision collided
        readInput
        ld A [delay]
        dec A
        ld [delay] A
        jp NZ done
        ldVia A [delay] [levelDelay]
        gravity
        jp done
        collided <- labelled do
            commitPiece
            removeFullLines
            newPiece
    pure ()
  where
    readInput = skippable \end -> do
        ld A [0x3adf]
        rec
            -- Check "h"
            rra
            jr NC moveLeft
            -- Check "i"
            rra
            -- jr NC rotate -- TODO
            -- Check "j"
            rra
            -- jr NC moveDown -- TODO
            -- Check "l"
            rra
            jr NC moveRight
            jp end

            moveLeft <- labelled do
                let withShiftedPiece :: (Load r [HL], RotateShift r) => r -> Z80ASM -> Z80ASM
                    withShiftedPiece r body = do
                        ld HL pieceBuf
                        decLoopB 4 do
                            ld r [HL]
                            sla r
                            body
                            inc HL
                            ld r [HL]
                            rl r
                            body
                            inc HL

                -- Set up DE to point to falling piece's line in the well
                ld DE wellContents
                ldVia A B [fallHeight]
                withLabel \loop -> do
                    replicateM_ 2 $ inc DE
                    djnz loop

                withShiftedPiece C do
                    ld A [DE]
                    inc DE
                    Z80.and C
                    jp NZ end
                withShiftedPiece A do
                    ld [HL] A
                jp end

            moveRight <- labelled do
                let withShiftedPiece :: (Load r [HL], RotateShift r) => r -> Z80ASM -> Z80ASM
                    withShiftedPiece r body = do
                        ld HL (pieceBuf + 7)
                        decLoopB 4 do
                            ld r [HL]
                            sra r
                            body
                            dec HL
                            ld r [HL]
                            rr r
                            body
                            dec HL

                -- Set up DE to point to falling piece's line in the well
                ld DE (wellContents + 7)
                ldVia A B [fallHeight]
                withLabel \loop -> do
                    replicateM_ 2 $ inc DE
                    djnz loop

                withShiftedPiece C do
                    ld A [DE]
                    dec DE
                    Z80.and C
                    jp NZ end
                withShiftedPiece A do
                    ld [HL] A
        pure ()

    gravity = do
        ld A [fallHeight]
        inc A
        ld [fallHeight] A

    checkCollision ifCollision = do
        ld HL pieceBuf
        ld DE wellContents
        ldVia A B [fallHeight]
        inc B
        withLabel \loop -> do
            replicateM_ 2 $ inc DE
            djnz loop
        decLoopB 8 do
            ldVia A C [HL]
            ld A [DE]
            Z80.and C
            jp NZ ifCollision
            inc HL
            inc DE

    commitPiece = do
        ld HL pieceBuf
        ld DE wellContents
        ldVia A B [fallHeight]
        withLabel \loop -> do
            replicateM_ 2 $ inc DE
            djnz loop
        decLoopB 8 do
            ldVia A C [HL]
            ld A [DE]
            Z80.or C
            ld [DE] A
            inc HL
            inc DE

    removeFullLines = do
        ld HL $ wellContents + wellHeight * 2
        decLoopB wellHeight $ skippable \next -> do
            dec HL
            ld E [HL]
            dec HL
            ld D [HL]

            ld A E
            cp 0xff
            jp NZ next
            ld A D
            cp 0xf0
            jp NZ next


            -- inc HL
            -- ldVia A [HL] 0
            -- inc HL
            -- ldVia A [HL] 0
            -- dec HL
            -- dec HL

            ld D H
            ld E L
            push BC
            withLabel \loop -> do
                ld A [DE]
                inc DE
                inc DE
                ld [DE] A
                dec DE
                dec DE

                dec DE

                ld A [DE]
                inc DE
                inc DE
                ld [DE] A
                dec DE
                dec DE

                dec DE

                djnz loop
            pop BC

    newPiece = do
        ldVia A [currentPiece] 1
        ldVia A [currentRot] 2
        ldVia A [currentPos] 4
        ldVia A [fallHeight] 0
        loadFallingPiece locs
        forM_ [0..7] \i -> do
            ldVia A [pieceBuf + i] [HL]
            inc HL

waitFrame :: Z80ASM
waitFrame = do
    halt

testState :: [Word16]
testState = map (`shiftL` 4) $
  [ 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00000_00000_1
  , 0b1_00011_11000_1
  , 0b1_01001_11100_1
  , 0b1_11111_11100_1
  , 0b1_11111_11110_1
  , 0b1_11111_11111_1
  ]
