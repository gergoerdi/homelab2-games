{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecursiveDo, LambdaCase #-}
module TVC.Hello where

import Z80
import Z80.Utils
import TVC

import Control.Monad
import Data.Word
import Data.Bits (shiftL, (.|.), (.&.))
import Control.Lens (toListOf)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Char (ord, isLower)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)

bufRows :: Word16
bufRows = 14

charsPerRow :: Word8
charsPerRow = 31

firstLine :: Word8
firstLine = 12

lastLine :: Word8
lastLine = 30

charHeight :: Word8
charHeight = 8

maxInput :: Word8
maxInput = charsPerRow - 1

rowStride :: Word16
rowStride = 64

kbdBufLen :: Word8
kbdBufLen = 4 -- Has to be a power of 2

hello :: BS.ByteString -> BS.ByteString -> Z80ASM
hello charset pic = mdo
    let printCharC = call myPrintCharCMode4

    di

    -- Save current graphics settings
    ld A [0x0b13]

    -- Set video mode 4
    ld C 1
    syscall 4
    setInterruptHandler handler
    ei

    -- Set palette 1 (foreground) for text
    ld A 0b11_11_11_11
    out [0x61] A

    -- Set palette 2 (user input)
    ld A 0b00_00_00_00
    out [0x62] A

    -- Clear screen
    syscall 0x05

    -- -- Set border color
    ld A 0b00_00_10_00
    out [0x00] A

    ld HL picData
    ld A 0b00_11_11_00
    call displayPicture

    ldVia A [lineNum] firstLine
    ldVia A [colNum] 0

    -- ld C 0
    -- replicateM_ 8 do
    --     decLoopB 16 do
    --         push BC
    --         printCharC
    --         pop BC
    --         inc C
    --     push BC
    --     call newLine
    --     pop BC
    -- loopForever $ pure ()

    call setMainColor
    ld HL str
    call printStr
    call newLine
    call newLine

    ld HL str'
    call printStr
    call newLine
    call newLine

    loopForever do
        call newLine
        ld HL inputBuf
        call inputLine
        call newLine
        call newLine
        ld HL str2
        call printStr
        call newLine

    loopForever $ pure ()

    pageVideoIn <- labelled do
        ld A 0x50
        ld [0x03] A
        out [0x02] A
        ret

    pageVideoOut <- labelled do
        ld A 0x70
        ld [0x03] A
        out [0x02] A
        ret

    -- Pre: HL is the start of the 80x40 pixel picture data
    -- Pre: A is the background color
    displayPicture <- labelled do
        push AF
        call pageVideoIn
        pop AF

        -- Set palette 0 (background) for text
        out [0x60] A

        -- Fill background of picture area
        ld DE videoStart
        decLoopB 90 do
            push BC
            decLoopB 64 do
                ld [DE] A
                inc DE
            pop BC
        let nextRow = do
                ld A (64 - 40)
                add A E
                ld E A
                unlessFlag NC $ inc D

        -- Draw picture
        ld DE $ videoStart + (8 * 64) + ((64 - 40) `div` 2)
        decLoopB 40 do
            push BC

            push HL
            ld BC 40
            ldir
            nextRow
            pop HL
            ld BC 40
            ldir
            nextRow

            pop BC
        jp pageVideoOut

    let setupLineInt y = do
            let (lo, hi) = wordBytes $ y * 16 {-+ 63-} -- - 46
            crtcOut 0x0e hi
            crtcOut 0x0f lo

    whichHalf <- labelled $ db [0]

    handler <- labelled mdo
        push AF
        push BC
        push DE
        push HL
        push IX
        out [0x07] A

        ld A [whichHalf]
        xor 0xff
        ld [whichHalf] A
        jp Z half2

        half1 <- labelled mdo
            when False do
                -- Set border color to dark green
                ld A 0b00_10_00_00
                out [0x00] A

            ld A [0x0b13]
            Z80.and 0b1111_1100
            Z80.or  0b0000_0010 -- Graphics mode 16
            out [0x06] A

            setupLineInt 87

            -- Scan keyboard
            ld A [0x0b11]
            Z80.and 0xf0
            ld C A
            ld HL kbdState
            decLoopB 10 do
                ld A C
                inc C

                out [0x03] A
                in_ A [0x58]

                ld [HL] A
                inc HL

            -- Compare keyboard state with previous state
            ld HL kbdState
            ld IX kbdPrevState
            ld C 0
            decLoopB 10 do
                ld A [HL]
                cpl
                Z80.and [IX]
                replicateM_ 8 do
                    srl A
                    jp C found
                    inc C
                inc HL
                inc IX
            jp copyKbdState

            found <- labelled do
                ld A [kbdBufW]
                inc A
                Z80.and $ kbdBufLen - 1
                ld [kbdBufW] A

                ld HL kbdBuf
                add A L
                ld L A
                unlessFlag NC $ inc H
                ld [HL] C

            copyKbdState <- labelled do
                ld DE kbdPrevState
                ld HL kbdState
                ld BC 10
                ldir

            when False do
                -- Set border color to dark green
                ld A 0b10_10_00_00
                out [0x00] A

            jp finish

        half2 <- labelled do
            when False do
                -- Set border color to red
                ld A 0b00_00_10_00
                out [0x00] A

            decLoopB 20 $ pure ()

            ld A [0x0b13]
            Z80.and 0b1111_1100
            Z80.or  0b0000_0001 -- Graphics mode 4
            -- Z80.or  0b0000_0000 -- Graphics mode 2
            out [0x06] A

            setupLineInt 239

            when False do
                -- Set border color to bright red
                ld A 0b10_00_10_00
                out [0x00] A

        finish <- label

        pop IX
        pop HL
        pop DE
        pop BC
        pop AF
        ei
        ret

    toHex <- labelled mdo
        cp 10
        jp NC hex
        add A $ tvcChar '0'
        ret
        hex <- label
        add A $ tvcChar 'a' - 10
        ret

    -- Input one line of text, store result in `[HL]`
    -- Mangles `HL`, `A`, and `B`
    inputLine <- labelled do
        -- Set color for user input
        call setInputColor

        -- Draw prompt
        push HL
        ld C $ tvcChar '>'
        printCharC
        pop HL

        ld B maxInput
        withLabel \loop -> mdo
            ld [HL] 0xff

            push BC
            push HL

            push BC
            ld C $ tvcChar '_'
            printCharC
            call printBack
            pop BC

            withLabel \waitForInput -> do
                call readChar
                jp Z waitForInput
            pop HL
            pop BC

            cp $ tvcChar '\n' -- End of line
            jr Z enter
            cp $ tvcChar '\DEL' -- Backspace
            jr Z backspace

            -- Normal character: print and record
            dec B
            jr Z noMoreRoom
            ld C A
            push BC
            push HL
            printCharC
            pop HL
            pop BC
            ld [HL] A
            inc HL
            jr loop

            noMoreRoom <- labelled do
                inc B -- So that next `dec B` will trigger `Z` again
            --     dec HL
            --     ld [HL] A

            --     -- Erase previous character

            --     ld A 0x07 -- Erase previous last character
            --     rst 0x28
            --     ld A [HL] -- Print new last character
            --     inc HL
            --     rst 0x28
                jr loop

            backspace <- labelled do
                -- Try to increase B
                inc B
                skippable \inRange -> do
                    ld A B
                    cp (maxInput + 1)
                    jr NZ inRange
                    dec B
                    jr loop

                -- Replace last printed character with a space
                push HL
                push BC
                ld C $ tvcChar ' '
                printCharC
                replicateM_ 2 $ call printBack
                ld C $ tvcChar ' '
                printCharC
                call printBack
                pop BC
                pop HL

                ld [HL] 0x00
                dec HL
                jr loop

            enter <- labelled do
                ld [HL] 0x20
                inc HL
                ld [HL] 0xff

                -- Remove cursor
                ld C $ tvcChar ' '
                printCharC

                -- Restore color
                jp setMainColor
            pure ()

    newLine <- labelled mdo
        ldVia A [colNum] 0
        ld A [lineNum]
        inc A
        cp lastLine
        jp Z scrollUp
        ld [lineNum] A
        ret
        scrollUp <- label
        call pageVideoIn
        ld HL $ videoStart + (fromIntegral firstLine + 1) * rowStride * fromIntegral charHeight
        ld DE $ videoStart + fromIntegral firstLine * rowStride * fromIntegral charHeight
        ld BC $ (fromIntegral $ lastLine - firstLine) * rowStride * fromIntegral charHeight
        ldir
        call pageVideoOut
        ret

    printByte <- labelled do
        push BC

        push AF
        Z80.and 0xf0
        replicateM_ 4 $ srl A
        call toHex

        ld C A
        printCharC

        pop AF
        Z80.and 0x0f
        call toHex

        ld C A
        printCharC

        pop BC
        ret

    -- Post: `A` is character, flag `Z` iff no character is available
    readChar <- labelled do
        ld A [kbdBufR]
        inc A
        Z80.and $ kbdBufLen - 1
        ld [kbdBufR] A

        ld HL kbdBuf
        add A L
        ld L A
        unlessFlag NC $ inc H
        ld A [HL]
        ld [HL] 0xff
        cp 0xff
        ret Z

        -- push AF
        -- call printByte
        -- pop AF

        ld HL keyData
        add A L
        ld L A
        unlessFlag NC $ inc H
        ld A [HL]
        cp 0x00
        ret

    printBack <- labelled do
        ld A [colNum]
        dec A
        -- dec A
        -- unlessFlag NC $ ld A 0
        ld [colNum] A
        ret

    -- Pre: `C` is the character to print
    -- Clobbers `AF`, `BC`, `HL`, `IX`
    myPrintCharCMode4 <- labelled mdo
        -- A = 8 * lineNum
        ld A [lineNum]
        sub 0
        replicateM_ 3 rla

        -- HL = 64 * (8 * lineNum)
        ld H 0
        ld L A
        decLoopB 6 do
            sla L
            rl H

        -- HL += 2 * colNum
        ld A [colNum]
        sla A
        inc A
        add A L
        unlessFlag NC $ inc H
        ld L A

        -- HL += videoStart
        ld DE videoStart
        add HL DE

        -- Increase column number
        -- TODO: newLine when needed
        ld A [colNum]
        inc A
        ld [colNum] A

        -- Search for character start address
        ld D 0
        ld E C
        sub 0
        replicateM_ 3 $ do
            rl E
            rl D
        ld IX charsetData
        add IX DE

        call pageVideoIn

        ld A [drawColorIsInput]
        cp 0x00
        ld DE $ rowStride - 1
        jp NZ drawInput

        drawOutput <- labelled do
            decLoopB 8 mdo
                ld A [IX]
                inc IX
                ld C A

                -- Byte 1: First four pixels
                Z80.and 0xf0
                ld [HL] A
                inc HL

                ld A C
                replicateM_ 4 rla
                Z80.and 0xf0

                ld [HL] A
                add HL DE
            jp drawDone

        drawInput <- labelled do
            decLoopB 8 mdo
                ld A [IX]
                inc IX
                ld C A

                -- Byte 1: First four pixels
                replicateM_ 4 $ srl A
                ld [HL] A
                inc HL

                ld A C
                Z80.and 0x0f
                -- replicateM_ 4 rla
                -- Z80.and 0xf0

                ld [HL] A
                add HL DE
            jp drawDone


        drawDone <- label
        jp pageVideoOut
        -- syscall 0x01
        ret

    setMainColor <- labelled do
        ldVia A [drawColorIsInput] 0x00
        ret

    setInputColor <- labelled do
        ldVia A [drawColorIsInput] 0xff
        ret

    printStr <- labelled do
        withLabel \loop -> mdo
            ld A [HL]
            cp 0xff
            ret Z

            inc HL
            cp $ tvcChar '\n'
            jp Z isNewLine

            ld C A
            push HL
            printCharC
            pop HL
            jp loop

            isNewLine <- label
            call newLine
            jp loop
        pure ()

    str <- labelled $ db $ (<> [0xff]) $ map tvcChar $ intercalate "\n"
      [ "Egy barátságos kis szobában"
      , "vagy. Egy fiatal srác ül egy"
      , "TVC előtt. Hirtelen hátrafordul"
      , "és beszélni kezd:"
      ]
      -- [ "Visszanyeri az eszméletét és"
      -- , "halkan beszélni kezd: Szörnyű"
      -- , "mészárlás volt... Megöltek"
      -- , "mindenkit a faluban... A"
      -- , "vezetőjüket Hakainak"
      -- , "szólították... Állj bosszút,"
      -- , "fiam... - Félrebillen a feje..."
      -- , "Halott..."
      -- ]
    str' <- labelled $ db $ (<> [0xff]) $ map tvcChar $ intercalate "\n"
      [ "Oh! Egy új játékos! Köszöntelek"
      , "a programomban! A küldetésed"
      , "akkor kezdődik, mikor kilépsz a"
      , "mögötted lévő ajtón! Beszélj"
      , "mindenkivel és mindent vizsgálj"
      , "meg!"
      ]
    str2 <- labelled $ db $ (<> [0xff]) $ map tvcChar $
      "Nem értem, próbálkozz mással!"

    keyData <- labelled $ db $ toByteMap keymap
    charsetData <- labelled $ db charset
    picData <- labelled $ db pic

    kbdPrevState <- labelled $ db $ replicate 10 0x00
    kbdState <- labelled $ db $ replicate 10 0x00
    kbdBuf <- labelled $ db $ replicate (fromIntegral kbdBufLen) 0xff
    kbdBufW <- labelled $ db [0]
    kbdBufR <- labelled $ db [0]
    lineNum <- labelled $ db [0]
    colNum <- labelled $ db [0]
    drawColorIsInput <- labelled $ db [0]

    inputBuf <- labelled $ db $ replicate (fromIntegral maxInput) 0xff

    pure ()

toByteMap :: [(Word8, Word8)] -> BS.ByteString
toByteMap vals = BS.pack [ fromMaybe 0 val | addr <- [0..255], let val = lookup addr vals ]

keymap :: [(Word8, Word8)]
keymap = map (tvcChar <$>)
    [ (0x06, '1')
    , (0x02, '2')
    , (0x01, '3')
    , (0x07, '4')
    , (0x00, '5')
    , (0x04, '6')
    , (0x0f, '7')
    , (0x09, '8')
    , (0x0a, '9')
    , (0x03, '0')
    , (0x28, '\DEL')
    , (0x16, 'q')
    , (0x12, 'w')
    , (0x11, 'e')
    , (0x17, 'r')
    , (0x10, 't')
    , (0x36, 'y')
    , (0x1f, 'u')
    , (0x19, 'i')
    , (0x1a, 'o')
    , (0x1e, 'p')
    , (0x26, 'a')
    , (0x22, 's')
    , (0x21, 'd')
    , (0x27, 'f')
    , (0x20, 'g')
    , (0x24, 'h')
    , (0x2f, 'j')
    , (0x29, 'k')
    , (0x2a, 'l')
    , (0x14, 'z')
    , (0x32, 'x')
    , (0x31, 'c')
    , (0x37, 'v')
    , (0x30, 'b')
    , (0x34, 'n')
    , (0x3f, 'm')
    , (0x3d, ' ')
    , (0x2c, '\n')
    , (0x39, ',')
    , (0x3a, '.')
    ]

tvcChar :: Char -> Word8
tvcChar = \case
    '\n' -> 0xf0
    'Á' -> 0x6d
    'É' -> 0x6c
    'Í' -> 0x61
    'Ó' -> 0x76
    'Ö' -> 0x75
    'Ő' -> 0x1b
    'Ú' -> 0x64
    'Ü' -> 0x40
    'Ű' -> 0x1b
    'á' -> 0x70
    'é' -> 0x71
    'í' -> 0x62
    'ó' -> 0x79
    'ö' -> 0x00
    'ő' -> 0x64
    'ú' -> 0x78
    'ü' -> 0x2a
    'ű' -> 0x5f
    '_' -> 0x6f
    c | isLower c -> fromIntegral (ord c) - 0x60
      | otherwise -> fromIntegral (ord c)
