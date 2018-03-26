module Scrabblatan.Scrabble.Tile
  ( Character (..)
  , Tile (..)
  , TileValues
  , matches
  ) where
import           Control.Arrow (first)
import           Data.Char     (toUpper)

data Character = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Eq, Read, Show)

data Tile = Blanco | Regular { character :: Character }
  deriving (Eq)

type TileValues = [(Tile, Int)]

matches :: Character -> Tile -> Bool
matches _ Blanco        = True
matches c1 (Regular c2) = c1 == c2

instance Read Tile where
  readsPrec _ ('B':'l':'a':'n':'c':'o':s) = return (Blanco, s)
  readsPrec n (c:cs)                      = first Regular <$> readsPrec n (toUpper c : cs)
  readsPrec _ _                           = []

instance Show Tile where
  show Blanco      = " |_| "
  show (Regular c) = " |" ++ show c ++ "| "
