module Scrabblatan.Scrabble.Rack
  ( Rack
  , remove
  ) where

import qualified Data.List                 as List

import           Scrabblatan.Scrabble.Tile

type Rack = [ Tile ]

remove :: Rack -> Tile -> Rack
remove rack c = List.delete c rack
