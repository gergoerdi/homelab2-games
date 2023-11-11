{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecursiveDo, LambdaCase #-}
module TVC.Hello where

import Z80
import Z80.Utils
import TVC

import Control.Monad
import Codec.Picture
import Data.Word
import Data.Bits (shiftL, (.|.), (.&.))
import Control.Lens (toListOf)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Char (ord)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)

bufRows :: Word16
bufRows = 14

charsPerRow :: Word8
charsPerRow = 32

maxInput :: Word8
maxInput = charsPerRow - 1

hello :: Image PixelRGB8 -> Z80ASM
hello pic = mdo
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

    -- Set border color to green
    ld A 0b10_10_00_00
    out [0x00] A

    ld HL picData
    ld A 0b00_11_00_00
    call displayPicture

    -- -- Print into text buffer
    -- ld DE textBuf
    -- replicateM_ 20 do
    --     ld HL str
    --     ld BC 13
    --     ldir

    ldVia A [lineNum] 0x0b
    ldVia A [colNum] 0x00
    ld BC 0x010b
    syscall 0x03
    ld HL str
    call printStr

    call newLine
    call newLine
    ld HL inputBuf
    call inputLine
    call newLine
    call newLine
    ld HL str2
    call printStr


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

        -- Set palette 0 (background) for text
        ld A 0b00_11_00_00
        out [0x60] A

        pop AF

        ld DE videoStart
        decLoopB 98 do
            push BC
            decLoopB 64 do
                ld [DE] A
                inc DE
            pop BC
        let nextRow = do
                ld A E
                add A (64 - 40)
                ld E A
                unlessFlag NC $ inc D

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

            setupLineInt 95

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
                Z80.and 0b1
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
        add A 0x30
        ret
        hex <- label
        add A $ 0x61 - 10
        ret

    -- Input one line of text, store result in `[HL]`
    -- Mangles `HL`, `A`, and `B`
    inputLine <- labelled do
        push HL
        ldVia A C [lineNum]
        ld B 0x01
        syscall 0x03

        -- Set color for user input
        ldVia A [0x0b4d] 2

        -- Draw prompt
        ld C $ fromIntegral . ord $ '>'
        printCharC
        pop HL

        ld B maxInput
        withLabel \loop -> mdo
            ld [HL] 0xff

            push BC
            push HL

            push BC
            ld C $ fromIntegral . ord $ '_'
            printCharC
            pop BC
            push BC
            call printBack
            pop BC

            withLabel \waitForInput -> do
                call readChar
                jp Z waitForInput
            pop HL
            pop BC

            cp $ fromIntegral . ord $ '\n' -- End of line
            jr Z enter
            cp $ fromIntegral . ord $ '\DEL' -- Backspace
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
                ld C $ fromIntegral . ord $ ' '
                printCharC
                pop BC
                push BC
                call printBack
                ld C $ fromIntegral . ord $ ' '
                printCharC
                pop BC
                push BC
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
                call printBack
                ld C $ fromIntegral . ord $ ' '
                printCharC

                -- Restore color
                ldVia A [0x0b4d] 1
                ret

            printBack <- labelled do
                ldVia A C [lineNum]
                ld A (maxInput + 2)
                sub B
                ld B A
                syscall 0x03
                ret

            pure ()

    newLine <- labelled do
        ldVia A [colNum] 0
        ld A [lineNum]
        inc A
        ld [lineNum] A
        ld C A
        ld B 1
        syscall 0x03
        ret

    printByte <- labelled do
        push BC

        push AF
        Z80.and 0xf0
        replicateM_ 4 $ srl A
        call toHex

        ld C A
        syscall 0x01

        pop AF
        Z80.and 0x0f
        call toHex

        ld C A
        syscall 0x01

        pop BC
        ret

    -- Post: `A` is character, flag `Z` iff no character is available
    readChar <- labelled do
        ld A [kbdBufR]
        inc A
        Z80.and 0b1
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

    myPrintCharCMode4 <- labelled $ do
        syscall 0x01
        ret

    printStr <- labelled $ do
        ld D charsPerRow
        withLabel \loop -> mdo
            ld A [HL]
            cp 0x00
            ret Z

            inc HL
            cp $ fromIntegral . ord $ '\n'
            jp Z isNewLine

            ld C A
            push HL
            printCharC
            pop HL

            dec D
            jp NZ loop

            isNewLine <- label
            ld D charsPerRow
            call newLine
            jp loop
        pure ()

    str <- labelled $ db $ (<> [0x00]) $ map tvcChar $ intercalate "\n"
      [ "Visszanyeri az eszméletét és"
      , "halkan beszélni kezd: Szörnyű"
      , "mészárlás volt... Megöltek"
      , "mindenkit a faluban... A"
      , "vezetőjüket Hakainak"
      , "szólították... Állj bosszút,"
      , "fiam... - Félrebillen a feje..."
      , "Halott..."
      ]
    str2 <- labelled $ db $ (<> [0x00]) $ map tvcChar $
      "Nem értem, próbálkozz mással!"
    textBuf <- labelled $ db $ replicate (fromIntegral $ bufRows * fromIntegral charsPerRow) 0x20
    kbdPrevState <- labelled $ db $ replicate 10 0x00
    kbdState <- labelled $ db $ replicate 10 0x00
    kbdBuf <- labelled $ db $ replicate 2 0xff
    kbdBufW <- labelled $ db [0]
    kbdBufR <- labelled $ db [0]
    lineNum <- labelled $ db [0]
    colNum <- labelled $ db [0]

    keyData <- labelled $ db $ toByteMap keymap

    picData <- labelled $ db
        [ interleave p1 p2
        | [p1, p2] <- chunksOf 2 $ map rgb2 $ toListOf imagePixels pic
        ]

    inputBuf <- labelled $ db $ replicate (fromIntegral maxInput) 0xff

    pure ()

interleave :: Word8 -> Word8 -> Word8
interleave x y = (x' `shiftL` 1) .|. y'
  where
    x' = spread x
    y' = spread y

    -- https://graphics.stanford.edu/~seander/bithacks.html#InterleaveBMN
    spread b = foldr (\(s, m) b -> (b .|. (b `shiftL` s)) .&. m) b (zip [1, 2, 4] [0x55, 0x33, 0x0f])

rgb2 :: PixelRGB8 -> Word8
rgb2 (PixelRGB8 r g b) = 0b1000 .|. (bit g `shiftL` 2) .|. (bit r `shiftL` 1) .|. bit b
  where
    bit 0x00 = 0
    bit 0xff = 1

toByteMap :: [(Word8, Word8)] -> BS.ByteString
toByteMap vals = BS.pack [ fromMaybe 0 val | addr <- [0..255], let val = lookup addr vals ]

keymap :: [(Word8, Word8)]
keymap = map (fromIntegral . ord <$>)
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
    'Á' -> 0x80
    'É' -> 0x81
    'Í' -> 0x82
    'Ó' -> 0x83
    'Ö' -> 0x84
    'Ő' -> 0x85
    'Ú' -> 0x86
    'Ü' -> 0x87
    'Ű' -> 0x88
    'á' -> 0x90
    'é' -> 0x91
    'í' -> 0x92
    'ó' -> 0x93
    'ö' -> 0x94
    'ő' -> 0x95
    'ú' -> 0x96
    'ü' -> 0x97
    'ű' -> 0x98
    c -> fromIntegral . ord $ c
