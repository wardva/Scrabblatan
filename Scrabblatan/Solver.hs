module Scrabblatan.Solver where

import           Control.Monad          (guard, when)

import qualified Scrabblatan.Dictionary as Dict
import           Scrabblatan.Scrabble
import qualified Scrabblatan.SuffixMap  as Suff

possibleMoves :: Suff.SuffixMap -> Dict.Dictionary -> Board -> Rack -> [(ScrabbleWord, Position, Direction)]
possibleMoves suffixTable dictionary board rack = do
  start     <- usablePositions board
  direction <- directions

  expandPosition suffixTable dictionary board rack direction start

expandPosition :: Suff.SuffixMap -> Dict.Dictionary -> Board -> Rack -> Direction -> Position -> [(ScrabbleWord, Position, Direction)]
expandPosition suffixTable dictionary board rack direction position = do
  rackTile <- rack

  let additional = getTileRow board position (swapDirection direction) rackTile

  when (length additional > 1) $
    let additionalExpanded = expandBlanco additional
     in guard $ any (`Dict.contains` dictionary) additionalExpanded

  suffix <- expandBlanco (getTileRow board position direction rackTile)

  guard $ Suff.contains suffix suffixTable

  let newRack = remove rack rackTile
  let newBoard = applyTile board position rackTile
  let nextFree = getNextFree board position direction

  let nextResults = nextFree >>= expandPosition suffixTable dictionary newBoard newRack direction

  if Dict.contains suffix dictionary
    then (suffix, position, direction) : nextResults
    else nextResults
