{-# LANGUAGE RecordWildCards #-}

module Scrabblatan.Scrabble.Move
  ( Move
  , applyMove
  , score
  ) where

import           Control.Arrow              (first)
import           Data.Maybe                 (catMaybes)
import qualified Data.Vector                as V

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

applyMove :: Board -> Move -> Board
applyMove board@Board {..} move = let changes = first (posToIndex boardSize) <$> move
                                      applied = V.accum (\cell tile -> cell { tile = Just tile }) getBoard changes
                                   in board { getBoard = applied }
