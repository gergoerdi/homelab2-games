module TVC.Hello.RatkaiPicture where

import qualified Data.ByteString as BS
import Data.Bits
import Data.Word

hiresPixels :: BS.ByteString -> BS.ByteString -> [Word8]
hiresPixels = hiresPixels' (10 * 8) (5 * 8) (10 * 8)

hiresPixels' :: Int -> Int -> Int -> BS.ByteString -> BS.ByteString -> [Word8]
hiresPixels' w h rowstride colors12 bitmap =
    concatMap (uncurry applyPalette) $
    zip (reorder rowstride w h $ toColorMap colors12) $
    reorder rowstride w h $ BS.unpack bitmap
  where
    toColorMap = concatMap (replicate 8) . BS.unpack

    applyPalette :: Word8 -> Word8 -> [Word8]
    applyPalette c12 = map toColor . bits
      where
        (c1, c2) = nybbles c12

        toColor True = c1
        toColor False = c2

reorder :: Int -> Int -> Int -> [a] -> [a]
reorder stride w h xs = map (xs!!) $
    [ (stride * y0) + (x * 8 + y)
    | y0 <- [0 .. (h `div` 8) - 1]
    , y <- [0..7]
    , x <- [0..(w `div` 8) -1]
    ]

bits :: Word8 -> [Bool]
bits b = map (testBit b) $ reverse [0..7]

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

nybbles :: Word8 -> (Word8, Word8)
nybbles b = both (.&. 0x0f) (n1, n0)
  where
    n1 = b `shiftR` 4
    n0 = b `shiftR` 0
