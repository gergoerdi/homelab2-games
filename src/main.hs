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

import qualified Data.ByteString as BS
import System.FilePath
import System.Directory
import Data.Word
import Data.Array (Array, (!), listArray)
import Data.Bits

main :: IO ()
main = do
    charSet <- BS.readFile "/home/cactus/prog/retro/homelab/ratkai/_obj/charset.bin"
    bs <- BS.drop 2 <$> BS.readFile "/home/cactus/prog/c64/bosszu-disasm/ram.mem"

    let picNum = 42
    -- let picNum = 1
    emit "_build/hello" $ TVC.Hello.hello charSet (picData picNum bs)

picWidth :: (Num a) => a
picWidth = 80

picHeight :: (Num a) => a
picHeight = 40

reorder :: Int -> Int -> Int -> [a] -> [a]
reorder stride w h xs = map (xs!!) $
    [ (stride * y0) + (x * 8 + y)
    | y0 <- [0 .. (h `div` 8) - 1]
    , y <- [0..7]
    , x <- [0..(w `div` 8) -1]
    ]

picData :: Word8 -> BS.ByteString -> BS.ByteString
picData i bs = colormap' <> reorder' bitmap
  where
    reorder' = BS.pack . reorder picWidth picWidth picHeight . BS.unpack

    size = picHeight * (picWidth `div` 8)
    bitmapAddr = 0xa000 + fromIntegral (i - 1) * (size + size `div` 8)
    colormapAddr = bitmapAddr + size
    bitmap = BS.take size . BS.drop bitmapAddr $ bs
    colormap = BS.take (size `div` 8) . BS.drop colormapAddr $ bs

    colormap' = BS.map toTVCColor colormap

    toTVCColor :: Word8 -> Word8
    toTVCColor = fromNybbles . both (tvcPalette !) . nybbles

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

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

nybbles :: Word8 -> (Word8, Word8)
nybbles b = both (.&. 0x0f) (n1, n0)
  where
    n1 = b `shiftR` 4
    n0 = b `shiftR` 0

fromNybbles :: (Word8, Word8) -> Word8
fromNybbles (n1, n0) = (n1 `shiftL` 4) .|. (n0 `shiftL` 0)
