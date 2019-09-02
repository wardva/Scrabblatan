{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Scrabblatan.Scrabble.Tile
  ( Character (..)
  , ScrabbleWord (..)
  , Tile (..)
  , TileValues
  , expandTile
  , readPTile
  , tileToChar
  ) where

import           Data.Functor                 (($>))
import           Data.Hashable                (Hashable)
import           GHC.Generics                 (Generic)
import           Text.ParserCombinators.ReadP

import           Scrabblatan.Scrabble.ScrabbleWord

data Tile = Blanco | Regular { character :: !Character }
  deriving (Eq, Generic, Hashable)

type TileValues = [(Tile, Int)]

expandTile :: Tile -> [Character]
expandTile (Regular c) = [c]
expandTile Blanco = allCharacters

readPTile :: ReadP Tile
readPTile = (char '_' $> Blanco) +++ (Regular <$> readPCharacter)

instance Read Tile where
  readsPrec _ = readP_to_S readPTile

instance Show Tile where
  show t = [tileToChar t]

tileToChar :: Tile -> Char
tileToChar Blanco      = '_'
tileToChar (Regular c) = characterToChar c
