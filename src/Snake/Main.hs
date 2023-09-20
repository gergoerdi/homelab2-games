{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Snake.Main (game) where

import Snake
import Snake.Transitions

import Z80
import Z80.Utils
import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char

wall :: Word8
wall = 0xfb
-- wall = 0xa0

fruit :: Word8
fruit = 0x74

life :: Word8
life = 0x0f

game :: Z80ASM
game = mdo
    let locs = MkLocs{..}

    loopForever do
        welcome locs

        call clearBufF
        call prepareGameF
        drawBorder
        call gameOverTransitionF

        skippable \gameOver -> loopForever mdo
            withLabel \loop -> mdo
                call drawUIF

                ld A [fruitNum]
                cp 10
                jp NZ stay

                finishLevel locs
                call prepareLevelF
                levelTransition locs
                jp newLevel

                stay <- label
                drawFruit locs
                call randomizeF
                ldVia A B [speed]
                srl B
                withLabel \loop -> do
                    call latchInputF
                    halt
                    call latchInputF
                    djnz loop
                ldVia A [currentDir] [lastInput]
                call moveF
                jp Z loop

            ld A [lives]
            Z80.and A
            jp Z gameOver
            dec A
            ld [lives] A
            call drawUIF

            call prepareLevelF
            deathTransition locs

            newLevel <- label
            pure ()

        gameOver locs
        call gameOverTransitionF
        call waitInputF

    moveF <- labelled $ move locs
    slitherF <- labelled $ slither locs
    latchInputF <- labelled $ latchInput locs
    lfsr10F <- labelled $ lfsr10
    rng <- labelled $ dw [0x0101]
    isInBoundsF <- labelled $ isInBounds locs
    randomizeF <- labelled $ randomize locs
    placeFruitF <- labelled $ placeFruit locs
    drawScoreF <- labelled $ do
        drawScore locs
        ret

    drawUIF <- labelled $ do
        drawUI locs
        ret

    clearBufF <- labelled do
        clearBuf
        ret
    waitInputF <- labelled do
        waitInput
        ret
    gameOverTransitionF <- labelled do
        gameOverTransition locs
        ret

    prepareGameF <- labelled do
        initGame locs
    prepareLevelF <- labelled do
        call clearBufF
        initLevel locs
        drawSnake locs
        drawFruitBuf locs
        ret

    bodyDispatchTrampoline <- labelled $ do
        ld HL [bodyDispatch]
        jp [HL] :: Z80ASM
    score <- labelled $ db [0, 0, 0]

    bodyDispatch <- labelled $ resw 1
    tailIdx <- labelled $ resb 1
    headIdx <- labelled $ resb 1
    newHead <- labelled $ resb 1
    segmentLo <- labelled $ resb 0x100
    segmentHi <- labelled $ resb 0x100
    segmentChar <- labelled $ resb 0x100
    lastInput <- labelled $ resb 1
    currentDir <- labelled $ resb 1
    fruitLoc <- labelled $ resw 1
    fruitNum <- labelled $ resb 1
    speed <- labelled $ resb 1
    lives <- labelled $ resb 1
    growth <- labelled $ resb 1
    pure ()

clearScreen :: Z80ASM
clearScreen = do
    ld HL videoStart
    withLabel \loop -> do
        ld [HL] space
        inc HL
        ld A H
        cp 0xc4
        jp NZ loop

clearBuf :: Z80ASM
clearBuf = do
    ld HL videoBufStart
    withLabel \loop -> do
        ld [HL] space
        inc HL
        ld A H
        cp 0x74
        jp NZ loop

showBuf :: Z80ASM
showBuf = do
    ld HL $ videoBufStart + 1 * numCols + 1
    ld DE $ videoStart + 1 * numCols + 1
    ld A $ numRows - 2
    withLabel \loop -> do
        ld BC $ numCols - 2
        ldir
        replicateM_ 2 $ inc HL
        replicateM_ 2 $ inc DE
        dec A
        jp NZ loop

initGame :: Locations -> Z80ASM
initGame MkLocs{..} = do
    ldVia A [speed] 10
    ldVia A [lives] 3
    ldVia A [fruitNum] 1
    ld A 0
    forM_ [0..2] \i -> ld [score + i] A

initLevel :: Locations -> Z80ASM
initLevel MkLocs{..} = do
    ldVia A [tailIdx] 0
    ldVia A [headIdx] 10
    ld A 0b1110_1111 -- L to move east
    ld [lastInput] A
    ld [currentDir] A

    ld A [fruitNum]
    dec A
    replicateM_ 3 $ sla A
    ld [growth] A

    ld HL segmentLo
    ld IX segmentHi
    ld IY segmentChar
    ld C 158
    decLoopB 11 do
        ld [HL] C
        inc C
        inc HL
        ldVia A [IX] 0xc1
        inc IX
        ldVia A [IY] bodyEW
        inc IY
    ldVia A [segmentChar + 10] headE
    call placeFruitF

placeFruit :: Locations -> Z80ASM
placeFruit MkLocs{..} = withLabel \loop -> do
    call randomizeF
    ldVia A [fruitLoc] [rng]
    ld L A
    ld A [rng + 1]
    Z80.and 0x03
    Z80.or 0xc0
    ld [fruitLoc + 1] A
    ld H A
    call isInBoundsF
    jp Z loop
    ret

-- | An 10-bit maximal LFSR
-- | Pre: `DE` is the current state
-- | Post: `DE` is the new state
-- | Clobbrs: `A`
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

randomize :: Locations -> Z80ASM
randomize MkLocs{..} = do
    ldVia A E [rng]
    ldVia A D [rng + 1]
    call lfsr10F
    ldVia A [rng] E
    ldVia A [rng + 1] D
    ret

-- | Pre: `HL` contains the address to check
-- | Post: `Z` iff out of bounds
isInBounds :: Locations -> Z80ASM
isInBounds MkLocs{..} = mdo
    -- Skip top border
    skippable \notTop -> do
        ld A H
        cp 0xc0
        jp NZ notTop
        ld A L
        cp (numCols + 1)
        jp C outOfBounds

    -- Skip bottom border
    skippable \notBottom -> do
        let lastRowStart = videoStart + (numRows - 1) * numCols + 1
            (lastRowStartL, lastRowStartH) = wordBytes lastRowStart
        ld A H
        cp lastRowStartH
        jp C notBottom
        jp NZ outOfBounds
        ld A L
        cp lastRowStartL
        jp NC outOfBounds

    -- Skip vertical borders
    -- Fast check: if last nybble is not 0, 8, 1 or 9, then definitely not on vertical border
    ld A L
    Z80.and 0b0000_0110
    ret NZ

    -- Slow check: keep subtracting `numCols / 2` from `HL / 2` until we get 0
    push HL
    push DE

    srl H
    rr L
    ld DE (negate $ numCols `div` 2)
    loopForever mdo
        ld A H
        cp $ 0xc0 `div` 2
        jp NZ next
        ld A L
        cp (numCols `div` 2)
        jp Z outOfBounds'

        -- If we've passed `numCols / 2`, we know we're inbound, and NZ was set previously. Otherwise, loop.
        jp NC next
        pop DE
        pop HL
        ret

        next <- label
        add HL DE

    outOfBounds' <- labelled do
        pop DE
        pop HL
    outOfBounds <- labelled $ do
        setZ
        ret
    pure ()

levelTransition :: Locations -> Z80ASM
levelTransition = transition 0x74 curtainV

deathTransition :: Locations -> Z80ASM
deathTransition = transition 0x79 curtainH

gameOverTransition :: Locations -> Z80ASM
gameOverTransition = transition 0x83 scramble


drawBorder :: Z80ASM
drawBorder = do
    drawBorderH
    drawBorderV
    drawText

drawBorderH :: Z80ASM
drawBorderH = do
    ld IX videoBufStart
    decLoopB numCols do
        ld [IX] wall
        inc IX
    ld IX (videoBufStart + (numRows - 1) * numCols)
    decLoopB numCols do
        ld [IX] wall
        inc IX

drawBorderV :: Z80ASM
drawBorderV = do
    ld IX (videoBufStart + numCols)
    ld DE (numCols - 1)
    decLoopB (numRows - 2) do
        ld [IX] wall
        add IX DE
        ld [IX] wall
        inc IX

drawText :: Z80ASM
drawText = do
    rec
        ld IX (videoBufStart + (numRows + 1) * numCols)
        ld IY text
        text <- stringLoopB "SCORE:" do
            ldVia A [IX] [IY]
            inc IX
            inc IY
    rec
        ld IX (videoBufStart + (numRows + 1) * numCols + 25)
        ld IY text
        text <- stringLoopB "LIVES:" do
            ldVia A [IX] [IY]
            inc IX
            inc IY
    pure ()

stringLoopB :: String -> Z80ASM -> Z80 Location
stringLoopB s body = skippable \end -> mdo
    decLoopB (fromIntegral $ length s) body
    jp end
    text <- labelled $ db $ map (fromIntegral . ord) s
    pure text

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
-- Post: `NZ` iff collision occurred
slither :: Locations -> Z80ASM
slither locs@MkLocs{..} = do
    ld B 0

    checkCollision
    -- `NC` if we ate a fruit
    skippable \noFruit -> do
        jp C noFruit
        exx
        incScore
        incFruit
        ld IX growth
        ld A [IX]
        add A 8
        ld [IX] A
        call placeFruitF
        exx

    skippable \grow -> mdo
        ld IX growth
        ld A [IX]
        cp 0
        jp Z noGrow
        dec [IX]
        jp grow
        noGrow <- labelled eraseTail
        pure ()

    replaceOldHead
    fillNewHead
    bumpHead

    -- Clear `NZ` if there was no collision
    setZ
    ret
  where
    checkCollision = skippable \eat -> do
        ldVia A C [headIdx]
        loadArray H (segmentHi, BC)
        loadArray L (segmentLo, BC)
        add HL DE

        -- Check collision
        ld A [HL]
        skippable \notFruit -> do
            cp $ succ . fromIntegral . ord $ '9'
            jp NC notFruit
            cp $ fromIntegral . ord $ '1'
            jp NC eat
        cp space
        ret NZ
        scf

    eraseTail = do
        ldVia A C [tailIdx]
        loadArray H (segmentHi, BC)
        loadArray L (segmentLo, BC)
        ld [HL] space

        inc A
        ld [tailIdx] A

    bumpHead = do
        ld HL headIdx
        ld [HL] C

    replaceOldHead = do
        ldVia A C [headIdx]
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

        -- Record new segment
        inc C
        writeArray (segmentHi, BC) H
        writeArray (segmentLo, BC) L

        -- New segment contents: head
        ld A [newHead]
        writeArray (segmentChar, BC) A

        -- Draw new head
        ld [HL] A

    incScore = mdo
        ld IX score
        skippable \finish -> decLoopB 3 do
            ld A [IX]
            inc A
            cp 10
            jp NZ finish
            ldVia A [IX] 0
            inc IX
        ld [IX] A

    incFruit = skippable \sameLevel -> do
        ld IX fruitNum
        ld A [IX]
        inc A
        ld [IX] A

setupSlither :: Locations -> (Int16, Int16) -> Word8 -> [(Word8, Word8)] -> Z80ASM
setupSlither locs@MkLocs{..} (dx, dy) newHeadChar bodyMap = do
    myDispatch <- skipped $ labelled do
        forM_ bodyMap \(head, body) -> do
            skippable \next -> do
                cp head
                jp NZ next
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
    ldVia A E [headIdx]

    withLabel \loop -> skippable \end -> do
        -- Set HL to target video address
        loadArray H (segmentHi, BC)
        ld A H
        Z80.and $ complement 0xc0
        Z80.or 0x70
        ld H A

        loadArray L (segmentLo, BC)

        -- Draw segment
        loadArray D (segmentChar, BC)
        ld [HL] D

        -- Compare iterator C with head E
        ld A E
        cp C
        jp Z end
        inc C
        jp loop

drawUI :: Locations -> Z80ASM
drawUI locs@MkLocs{..} = do
    ld IX (videoStart + (numRows + 1) * numCols + 10)
    call drawScoreF
    drawLives locs
    ret

drawScore :: Locations -> Z80ASM
drawScore MkLocs{..} = do
    ld IY (score + 2)
    decLoopB 3 do
        ld A [IY]
        add A $ fromIntegral . ord $ '0'
        ld [IX] A
        inc IX
        dec IY

drawLives :: Locations -> Z80ASM
drawLives MkLocs{..} = do
    ld IX (videoStart + (numRows + 1) * numCols + 33)
    ld A [lives]
    decLoopB 3 mdo
        cp B
        jp NC isLife
        ld [IX] space
        jp next

        isLife <- label
        ld [IX] life
        next <- label
        inc IX

drawFruit :: Locations -> Z80ASM
drawFruit MkLocs{..} = do
    ldVia A L [fruitLoc]
    ldVia A H [fruitLoc + 1]
    ld A [fruitNum]
    add A $ fromIntegral $ ord '0'
    ld [HL] A

drawFruitBuf :: Locations -> Z80ASM
drawFruitBuf MkLocs{..} = do
    ldVia A L [fruitLoc]
    ld A [fruitLoc + 1]
    Z80.and $ complement 0xc0
    Z80.or 0x70
    ld H A

    ld A [fruitNum]
    add A $ fromIntegral $ ord '0'
    ld [HL] A

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
    ld C A

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
        setZ
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
            ldVia A [lastInput] C
    ret

-- | Post: `NZ` iff collision
move :: Locations -> Z80ASM
move locs@MkLocs{..} = do
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
        setZ
        ret

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
    ret

finishLevel :: Locations -> Z80ASM
finishLevel locs@MkLocs{..} = do
    ld IX fruitNum
    ld A 1
    ld [IX] A

    ld IX speed
    ld A [IX]
    dec A
    ld [IX] A

printCenteredLine :: Location -> Location -> String -> Z80ASM
printCenteredLine base row s = mdo
    ld IX $ base + numCols * row + (numCols - fromIntegral (length s)) `div` 2
    ld IY text
    text <- stringLoopB s do
        ldVia A [IX] [IY]
        inc IX
        inc IY
    pure ()

printCenteredLines :: Location -> Location -> [String] -> Z80ASM
printCenteredLines base row = mapM_ (uncurry $ printCenteredLine base) . zip [row..]

gameOver :: Locations -> Z80ASM
gameOver locs@MkLocs{..} = do
    call clearBufF
    printCenteredLine videoBufStart 7 $ invert "GAME OVER"

    let s = "SCORE: "
    ld IX $ videoBufStart + numCols * 12 + (numCols - (fromIntegral (length s) + 3)) `div` 2
    rec
        ld IY text
        text <- stringLoopB s do
            ldVia A [IX] [IY]
            inc IX
            inc IY
    call drawScoreF

    printCenteredLine videoBufStart 22 "PRESS ANY DIRECTION TO START"

invert :: String -> String
invert = map (chr . (+ 0x80) . ord)

welcome :: Locations -> Z80ASM
welcome locs@MkLocs{..} = skippable \end -> mdo
    drawWelcome
    call gameOverTransitionF

    ldVia A [released] 0

    ldVia A [tailIdx] 0
    ldVia A [headIdx] 0
    ldVia A [growth] 8

    let pos = videoStart + numCols * 13 + 19
        (lo, hi) = wordBytes pos
    ldVia A [segmentLo] lo
    ldVia A [segmentHi] hi
    ldVia A [segmentChar] headE

    ld A 0b1110_1111
    ld [lastInput] A
    ld B 5
    call animate

    loopForever do
        ld A 0b1111_0111
        ld [lastInput] A
        ld B 8
        call animate

        ld A 0b1111_1011
        ld [lastInput] A
        ld B 10
        call animate

        ld A 0b1111_1101
        ld [lastInput] A
        ld B 8
        call animate

        ld A 0b1110_1111
        ld [lastInput] A
        ld B 10
        call animate

    call waitInputF
    jp end

    released <- labelled $ db [0]

    animate <- labelled do
        push BC
        call moveF

        decLoopB 4 do
            skippable \keepWaiting -> do
                skippable \checkPressed -> do
                    ld HL released
                    ld A [HL]
                    cp 0
                    jp Z checkPressed

                    ld A [0x3adf]
                    cpl
                    Z80.and 0b0001_1110
                    jp Z keepWaiting
                    inc [HL]
                    jp keepWaiting

                ld A [0x3adf]
                cpl
                Z80.and 0b0001_1110
                jp NZ end

            halt
        pop BC
        dec B
        jp NZ animate
        ret
    pure ()

drawWelcome :: Z80ASM
drawWelcome = do
    clearBuf
    printCenteredLines videoBufStart 5 lines
  where
    lines =
      [ invert "SNAKE"
      , " "
      , "GOBBLE UP ALL NUMBERED FRUIT"
      , "WITHOUT BITING YOUR OWN TAIL"
      , " "
      , "NEW LEVEL AFTER 9 FRUIT"
      , "SPEED GOES UP EVERY OTHER LEVEL"
      , " "
      , " "
      , "I"
      , " "
      , "\x04"
      , "J \x01\x73\x00 L"
      , "\x05"
      , " "
      , "K"
      , " "
      , "PRESS ANY DIRECTION TO START"
      ]

waitInput :: Z80ASM
waitInput = skippable \end -> do
    -- Wait for release
    withLabel \loop -> do
        ld A [0x3adf]
        cpl
        Z80.and 0b0001_1110
        jp NZ loop
    -- Wait for press
    withLabel \loop -> do
        ld A [0x3adf]
        cpl
        Z80.and 0b0001_1110
        jp Z loop
