{-# LANGUAGE LambdaCase #-}

module Scrabblatan.Scrabble.Tile
  ( Character (..)
  , ScrabbleWord (..)
  , Tile (..)
  , TileValues
  , matches
  , expandBlanco
  ) where

import           Control.Arrow    (first)
import           Data.Char        (toUpper)

import           Scrabblatan.Scrabble.Core

data Tile = Blanco | Regular { character :: Character }
  deriving (Eq)

type TileValues = [(Tile, Int)]

expandBlanco :: [Tile] -> [ScrabbleWord]
expandBlanco tiles = ScrabbleWord <$> traverse (\case Blanco       -> [A .. Z]
                                                      (Regular c)  -> [c]
                                               ) tiles

matches :: Character -> Tile -> Bool
matches c1 (Regular c2) = c1 == c2
matches _ Blanco        = True

instance Read Tile where
  readsPrec _ ('B':'l':'a':'n':'c':'o':s) = return (Blanco, s)
  readsPrec n (c:cs)                      = first Regular <$> readsPrec n (toUpper c : cs)
  readsPrec _ _                           = []

instance Show Tile where
  show Blanco      = " |_| "
  show (Regular c) = " |" ++ show c ++ "| "
