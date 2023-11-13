{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

-- import qualified Tetris.Main as Tetris
-- import qualified Snake.Main as Snake
-- import qualified HL2048.Main as HL2048
-- import qualified TrafficJam.Main as TrafficJam
import qualified TVC.Hello

import Z80
import Z80.Utils
import TVC
import TVC.Hello.RatkaiPicture -- XXX

import qualified Data.ByteString as BS
import System.FilePath
import System.Directory
import Text.Printf
import Data.Word
import Data.List.Split (chunksOf)
import Data.Array (Array, (!), listArray)
import Data.Bits

main :: IO ()
main = do
    charSet <- BS.readFile "/home/cactus/prog/retro/homelab/ratkai/_obj/charset.bin"
    bs <- BS.drop 2 <$> BS.readFile "/home/cactus/prog/c64/bosszu-disasm/ram.mem"

    let picNum = 42 :: Word8
    emit "_build/hello" $ TVC.Hello.hello charSet (picData picNum bs)

picData :: Word8 -> BS.ByteString -> BS.ByteString
picData i bs = BS.pack $ map toTVCColor . chunksOf 2 $ hiresPixels colors bitmap
  where
    bitmapAddr = 0xa000 + fromIntegral (i - 1) * 450
    colorsAddr = bitmapAddr + 0x190
    bitmap = BS.drop bitmapAddr bs
    colors = BS.drop colorsAddr bs

    toTVCColor :: [Word8] -> Word8
    toTVCColor [p1, p2] = interleave (tvcPalette ! p1) (tvcPalette ! p2)

    tvcPalette :: Array Word8 Word8
    tvcPalette = listArray (0, 15) . map toLGRB $
        [ (False, False, False, False)
        , (True,  True,  True,  True)
        , (True,  True,  False, True)
        , (True,  False, True,  True)
        , (False, True,  False, True)
        , (True,  False, True,  False)
        , (True,  False, False, True)
        , (True,  True,  True,  False)
        , (False, True,  True,  False)
        , (False, True,  False, False)
        , (True,  True,  False, False)
        , (False, True,  True,  False)
        , (False, True,  True,  True)
        , (False, True,  True,  False)
        , (False, False, True,  True)
        , (True,  True,  True,  True)
        ]

    toLGRB :: (Bool, Bool, Bool, Bool) -> Word8
    toLGRB (l, r, g, b) =
      (if l then 0b1000 else 0b0000) .|.
      (if g then 0b0100 else 0b0000) .|.
      (if r then 0b0010 else 0b0000) .|.
      (if b then 0b0001 else 0b0000)



emit :: String -> Z80ASM -> IO ()
emit name prog = do
    createDirectoryIfMissing True (takeDirectory name)
    BS.writeFile (name <.> "obj") $ asmData block
    BS.writeFile (name <.> "cas") $ cas block
  where
    block = org 0x1a00 prog

cas :: ASMBlock -> BS.ByteString
cas mainBlock = mconcat
    [ padTo 0x80 header
    , BS.singleton 0x00
    , contents
    ]
  where
    contents = blocksOf $ mconcat
        [ basic
        , asmData mainBlock
        ]

    blocksOf bs = mconcat
      [ BS.singleton 0x01 -- Type: program file
      , word $ fromIntegral $ BS.length bs
      , BS.singleton 0xff -- Auto-run: on
      , BS.replicate 10 0x00
      , BS.singleton 0x00 -- Version tag
      , bs
      ]

    header = mconcat
        [ BS.singleton 0x11 -- Buffered: no
        , BS.singleton 0x00 -- Copy-protecion: off
        , word (fromIntegral numBlocks)
        , BS.singleton (fromIntegral lastBlockSize)
        ]

    len = BS.length contents
    ((+ 1) -> numBlocks, (+ 1) -> lastBlockSize) = len `divMod` 0x80

    padTo n bs = BS.take n $ bs <> BS.replicate n 0x00

    basic = BS.pack
        [ 0x0f
        , 0x0a, 0x00, 0xdd, 0x20                      -- 10 PRINT
        , 0x55, 0x53, 0x52                            -- USR
        , 0x96, 0x36, 0x36, 0x35, 0x36, 0x95          -- (6656)
        , 0xff                                        -- end of BASIC line
        , 0x00                                        -- end of BASIC program
        , 0x00
        ]

    word w = BS.pack [lo, hi]
      where
        (lo, hi) = wordBytes w
