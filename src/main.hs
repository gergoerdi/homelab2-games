{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import qualified Tetris.Main as Tetris
import qualified Snake.Main as Snake
import qualified HL2048.Main as HL2048
import qualified TrafficJam.Main as TrafficJam
import qualified CHIP80.Main as CHIP80

import Z80
import Z80.Utils
import Z80.ZX0.Compress

import Data.Word
import qualified Data.ByteString as BS
import Control.Monad
import Data.Bits
import Data.Char
import Text.Printf
import Data.String (fromString)
import System.FilePath
import System.Directory

main :: IO ()
main = do
    emit "_build/tetris" $ org 20000 Tetris.game
    emit "_build/snake" $ org 20000 Snake.game
    emit "_build/hl2048" $ org 20000 HL2048.game
    -- emit "_build/trafficjam" $ org 20000 TrafficJam.game
    emit "_build/chip80" =<< org 20000 <$> CHIP80.withGamesFrom "data/chip80"

emit :: String -> ASMBlock -> IO ()
emit name block = do
    createDirectoryIfMissing True (takeDirectory name)
    BS.writeFile (name <.> "obj") $ asmData block
    BS.writeFile (name <.> "htp") $ htp (fromString $ takeBaseName name) block

htp :: BS.ByteString -> ASMBlock -> BS.ByteString
htp label mainBlock = mconcat
    [ leader
    , record label $ org 0x4002 do
            dw [asmOrg mainBlock]
    , BS.singleton 0x01
    , leader
    , record mempty mainBlock
    , BS.singleton 0x00
    ]
  where
    leader = BS.replicate 100 0x00

    record label block = mconcat
        [ BS.singleton 0xa5
        , label
        , BS.singleton 0x00
        , word $ asmOrg block
        , word . fromIntegral $ BS.length bs
        , bs
        , crc bs
        ]
      where
        bs = asmData block

    crc = BS.singleton . BS.foldr' (+) 0

    word w = BS.pack [lo, hi]
      where
        (lo, hi) = wordBytes w
