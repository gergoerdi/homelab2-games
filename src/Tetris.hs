{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Tetris where

import Z80
import Data.Word
import Data.Bits
import Data.Semigroup (stimes)

data Locations = MkLocs
  { pieces :: Location
  , wellContents :: Location
  , currentPiece, currentRot, currentPos :: Location
  , pieceBuf :: Location
  , lineBuf :: Location
  , fallHeight :: Location
  , delay :: Location
  }

padTo :: Int -> a -> [a] -> [a]
padTo n x0 [] = replicate n x0
padTo n x0 (x:xs) = x : padTo (n-1) x0 xs

dw :: [Word16] -> Z80ASM
dw = db . concatMap toBytes
  where
    toBytes w = [fromIntegral $ w `mod` 0x100, fromIntegral $ w `div` 0x100]

wellWidth :: Num a => a
wellWidth = 10

wellHeight :: Num a => a
wellHeight = 20

allPieces :: [Word16]
allPieces = concatMap (concatMap encode) [t, j, z, o, s, l, i]
  where
    normalize :: [String] -> [String]
    normalize = map (padTo 4 ' ') . padTo 4 ""

    encode :: [String] -> [Word16]
    encode = map ((`shiftL` 8) . stringToByte) . normalize

    stringToByte :: String -> Word16
    stringToByte = foldl (\w c -> let w' = w `shiftL` 1 in if c == ' ' then w' else setBit w' 0) 0x00

    t =
      [ [ "***"
        , " * "
        ]
      , [ " * "
        , " **"
        , " * "
        ]
      , [ " * "
        , "***"
        ]
      , [ " *"
        , "**"
        , " *"
        ]
      ]

    j =
      [ [ "***"
        , "  *"
        ]
      , [ " **"
        , " * "
        , " * "
        ]
      , [ "*  "
        , "***"
        ]
      , [ " *"
        , " *"
        , "**"
        ]
      ]
    l = map (map reverse) j

    z = stimes 2 $
      [ [ "** "
        , " **"
        ]
      , [ " *"
        , "**"
        , "* "
        ]
      ]
    s = map (map reverse) z

    o = stimes 4 $
      [ [ " ** "
        , " ** "
        ]
      ]

    i = stimes 2 $
      [ [ "****"
        ]
      , [ "*"
        , "*"
        , "*"
        , "*"
        ]
      ]
