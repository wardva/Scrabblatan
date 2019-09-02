{-# LANGUAGE RecordWildCards #-}

module Scrabblatan.Scrabble.Rack
  ( Rack (..)
  , rackLength
  , rackReadP
  , remove
  ) where

import qualified Data.List                    as List
import           Text.ParserCombinators.ReadP

import           Scrabblatan.Scrabble.Tile

newtype Rack = Rack { rackTiles :: [Tile] }
  deriving (Eq)

remove :: Rack -> Tile -> Rack
remove (Rack rack) c = Rack (List.delete c rack)

rackLength :: Rack -> Int
rackLength (Rack r) = length r

instance Show Rack where
  show Rack {..} = "[" ++ concatMap (\x -> tileToChar x : " ") rackTiles ++ "]"

rackReadP :: ReadP Rack
rackReadP = Rack <$>
  (char '[' <* skipSpaces *> many (readPTile <* skipSpaces) <* char ']')

instance Read Rack where
  readsPrec _ = readP_to_S rackReadP
