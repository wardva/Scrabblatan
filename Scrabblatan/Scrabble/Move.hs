module Scrabblatan.Scrabble.Move
  ( Move
  , score
  ) where

import           Data.Maybe                 (catMaybes)

import           Scrabblatan.Scrabble.Board
import           Scrabblatan.Scrabble.Bonus
import           Scrabblatan.Scrabble.Tile

type Move = [(Position, Tile)]

score :: Board -> TileValues -> Move -> Int
score board values move = let tileScores = fmap (uncurry tileScore) move
                              wordScore = applyCharBonusses tileScores
                              bonusses = catMaybes $ fmap snd tileScores
                           in applyWordBonusses wordScore bonusses
  where tileScore :: Position -> Tile -> (Int, Maybe Bonus)
        tileScore pos tile' = let (Just baseScore) = tile' `lookup` values
                               in (baseScore, getBonus board pos)
