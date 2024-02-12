{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import qualified Tetris.Main as Tetris
import qualified Snake.Main as Snake
import qualified HL2048.Main as HL2048
import qualified TrafficJam.Main as TrafficJam

import Z80
import Z80.Utils
import Z80.ZX0.Compress
import Z80.Machine.HomeLab.HTP

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
    emit2 "_build/tetris" $ org 20000 Tetris.game
    emit2 "_build/snake" $ org 20000 Snake.game
    emit2 "_build/hl2048" $ org 20000 HL2048.game
    -- emit "_build/trafficjam" $ org 20000 TrafficJam.game

emit2 :: String -> ASMBlock -> IO ()
emit2 name block = do
    createDirectoryIfMissing True (takeDirectory name)
    BS.writeFile (name <.> "obj") $ asmData block
    BS.writeFile (name <.> "htp") $ htp (fromString $ takeBaseName name)
      [ block
      , org 0x4002 $ dw [asmOrg block]
      ]
