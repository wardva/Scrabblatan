module Scrabblatan.Scrabble.Rack
  ( Rack
  , available
  , timesAvailable
  ) where
import Scrabblatan.Scrabble.Tile

type Rack = [ Tile ]

available :: Rack -> Character -> Bool
available rack c = any (matches c) rack

timesAvailable :: Rack -> Character -> Int
timesAvailable rack c = length $ filter (matches c) rack
