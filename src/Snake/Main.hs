{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Snake.Main (snake) where

import Z80
import Z80.Utils
import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char

data Locations = MkLocs
  { headIdx :: Location
  , tailIdx :: Location
  , segmentLo, segmentHi, segmentChar :: Location
  , newHead :: Location
  , bodyDispatchTrampoline :: Location
  , bodyDispatch :: Location
  , slitherF :: Location
  , lastInput, currentDir :: Location
  }

videoStart :: Word16
videoStart = 0xc001

numCols :: (Num a) => a
numCols = 40

numRows :: (Num a) => a
numRows = 25

space :: Word8
space = 0x20
-- space = 0xfb

wall :: Word8
wall = 0xfb
-- wall = 0xa0

snake :: Z80ASM
snake = do
    rec
        let locs = MkLocs{..}
        clearScreen
        drawBorder
        drawSnake locs
        loopForever $ do
            replicateM_ 3 do
                halt
                call latchInputF
            ldVia A [currentDir] [lastInput]
            readInput locs

        slitherF <- labelled $ slither locs
        latchInputF <- labelled $ latchInput locs

        tailIdx <- labelled $ db [0]
        headIdx <- labelled $ db [10]
        newHead <- labelled $ db [0]
        segmentLo <- labelled $ db $ take 256 $ [0..]
        segmentHi <- labelled $ db $ take 256 $ repeat 0xc1
        segmentChar <- labelled $ db $ take 256 $ replicate 10 bodyEW ++ [headE] ++ repeat 0
        bodyDispatchTrampoline <- labelled $ db [0xc3] -- JP
        bodyDispatch <- labelled $ dw [0]

        lastInput <- labelled $ db [0b1110_1111]
        currentDir <- labelled $ db [0b1110_1111]
    pure ()

clearScreen :: Z80ASM
clearScreen = do
    ld HL videoStart
    rec loop <- label
        ld [HL] space
        inc HL
        ld A H
        cp 0xc4
        jr NZ loop
    pure ()

drawBorder :: Z80ASM
drawBorder = do
    ld HL videoStart
    decLoopB numCols do
        ld [HL] wall
        inc HL
    ld DE (numCols - 1)
    decLoopB (numRows - 2) do
        ld [HL] wall
        add HL DE
        ld [HL] wall
        inc HL
    decLoopB numCols do
        ld [HL] wall
        inc HL

bodyEW, bodyNS, bodySE, bodyNE :: Word8
bodyEW = 0x91
bodyNS = 0x90
bodySE = 0x6e
bodyNE = 0x6c
bodySW = 0x6d
bodyNW = 0x6b

headS, headW, headN, headE :: Word8
-- headS = 0x1c
-- headW = 0x1a
-- headN = 0x1d
-- headE = 0x1b
headS = 0x71
headN = 0x72
headE = 0x70
headW = 0x6f

-- Pre: `DE` is the movement vector
-- Pre: `[bodyDispatch]` transforms `A` from current head to new body
slither :: Locations -> Z80ASM
slither MkLocs{..} = do
    ld B 0

    ldVia A C [tailIdx]
    eraseTail
    bumpTail

    ldVia A C [headIdx]

    replaceOldHead
    fillNewHead
    bumpHead
    ret
  where
    eraseTail = do
        loadArray H (segmentHi, BC)
        loadArray L (segmentLo, BC)
        ld [HL] space

    bumpTail = do
        inc A
        ld HL tailIdx
        ld [HL] A

    bumpHead = do
        ld HL headIdx
        ld [HL] C

    replaceOldHead = do
        loadArray A (segmentChar, BC)

        call bodyDispatchTrampoline
        writeArray (segmentChar, BC) A

        -- Redraw old head with body
        loadArray H (segmentHi, BC)
        loadArray L (segmentLo, BC)
        ld [HL] A

    fillNewHead = do
        -- New segment location: head + direction offset
        add HL DE
        inc C
        writeArray (segmentHi, BC) H
        writeArray (segmentLo, BC) L

        -- New segment contents: head
        ld A [newHead]
        writeArray (segmentChar, BC) A

        -- Draw new head
        ld [HL] A

setupSlither :: Locations -> (Int16, Int16) -> Word8 -> [(Word8, Word8)] -> Z80ASM
setupSlither locs@MkLocs{..} (dx, dy) newHeadChar bodyMap = do
    myDispatch <- skipped $ labelled do
        forM_ bodyMap \(head, body) -> do
            skippable \next -> do
                cp head
                jr NZ next
                ld A body
                ret
    let delta = dx + 40 * dy
    ld DE $ fromIntegral delta
    ldVia A [newHead] newHeadChar

    ld HL bodyDispatch
    let (lo, hi) = wordBytes myDispatch
    ldVia A [HL] lo
    inc HL
    ldVia A [HL] hi

drawSnake :: Locations -> Z80ASM
drawSnake MkLocs{..} = do
    ld B 0
    ldVia A C [tailIdx]
    ld A [headIdx]

    withLabel \loop -> skippable \end -> do
        -- Set HL to target video address
        loadArray H (segmentHi, BC)
        loadArray L (segmentLo, BC)

        -- Draw segment
        loadArray D (segmentChar, BC)
        ld [HL] D

        -- Compare iterator C with head A
        cp C
        jp Z end
        inc C
        jp loop

-- | Pre: register `idx` contains the index
-- | Post: `target` contains the value at `(base + index)`
loadArray :: (Load r [RegIx], Arithmetic RegIx r') => r -> (Location, r') -> Z80ASM
loadArray target (base, idx) = do
    ld IX base
    add IX idx
    ld target [IX]

writeArray :: (Load [RegIx] r, Arithmetic RegIx r') => (Location, r') -> r -> Z80ASM
writeArray (base, idx) src = do
    ld IX base
    add IX idx
    ld [IX] src

latchInput :: Locations -> Z80ASM
latchInput MkLocs{..} = do
    ld A [0x3adf]
    ld B A

    rec
        -- Check "i"
        rra
        rra
        jp NC moveV
        -- Check "j"
        rra
        jp NC moveH
        -- Check "k"
        rra
        jp NC moveV
        -- Check "l"
        rra
        jp NC moveH
        ret

        moveV <- labelled do
            ld A [currentDir]
            cp 0b1111_1101 -- I
            ret Z
            cp 0b1111_0111 -- K
            ret Z
            jp move
        moveH <- labelled do
            ld A [currentDir]
            cp 0b1111_1011 -- J
            ret Z
            cp 0b1110_1111 -- L
            ret Z
            jp move

        move <- labelled do
            ldVia A [lastInput] B
    ret

readInput :: Locations -> Z80ASM
readInput locs@MkLocs{..} = skippable $ \end -> do
    ld A [lastInput]
    rec
        -- Check "i"
        rra
        rra
        jp NC moveN
        -- Check "j"
        rra
        jp NC moveW
        -- Check "k"
        rra
        jp NC moveS
        -- Check "l"
        rra
        jp NC moveE
        jp end

        moveS <- labelled do
            setupSlither locs (0, 1) headS $
              [ (headS, bodyNS)
              , (headN, bodyNS)
              , (headW, bodySE)
              , (headE, bodySW)
              ]
            jp move
        moveW <- labelled do
            setupSlither locs (-1, 0) headW $
              [ (headS, bodyNW)
              , (headN, bodySW)
              , (headW, bodyEW)
              , (headE, bodyEW)
              ]
            jp move
        moveN <- labelled do
            setupSlither locs (0, -1) headN $
              [ (headS, bodyNS)
              , (headN, bodyNS)
              , (headW, bodyNE)
              , (headE, bodyNW)
              ]
            jp move
        moveE <- labelled do
            setupSlither locs (1, 0) headE $
              [ (headS, bodyNE)
              , (headN, bodySE)
              , (headW, bodyEW)
              , (headE, bodyEW)
              ]
            jp move

        move <- labelled do
            call slitherF

    pure ()
