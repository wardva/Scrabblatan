module Scrabblatan.Solver where

import           Control.Monad            (guard, when)
import           Control.Monad.State.Lazy
import qualified Data.HashSet             as Set

import qualified Scrabblatan.Dictionary   as Dict
import           Scrabblatan.Scrabble
import qualified Scrabblatan.SuffixMap    as Suff

type Result = (ScrabbleWord, Position, Direction)

type Discovered = Set.HashSet Result

solve :: Suff.SuffixMap -> Dict.Dictionary -> Board -> Rack -> [Result]
solve suffixMap dictionary board rack =
  evalStateT (possibleMoves suffixMap dictionary board rack) Set.empty

possibleMoves :: Suff.SuffixMap -> Dict.Dictionary -> Board -> Rack -> StateT Discovered [] Result
possibleMoves suffixTable dictionary board rack = do
  start     <- lift $ usablePositions board
  direction <- lift directions

  expandPosition suffixTable dictionary board rack direction start

expandPosition :: Suff.SuffixMap -> Dict.Dictionary -> Board -> Rack -> Direction -> Position -> StateT Discovered [] Result
expandPosition suffixTable dictionary board rack direction position = do
  rackTile <- lift (rackTiles rack)

  currentCharacter <- lift (expandTile rackTile)

  let additional = getRow board position (swapDirection direction) currentCharacter

  when (wordLength additional > 1) $
    guard $ additional `Dict.contains` dictionary

  let currentMove = getRow board position direction currentCharacter
  let discovery = (currentMove, position, direction)

  discovered <- get

  guard $ not (Set.member discovery discovered)

  let newDiscovered = Set.insert discovery discovered

  put newDiscovered

  guard $ Suff.contains currentMove suffixTable

  let newRack = remove rack rackTile
  let newBoard = applyCharacter board position currentCharacter
  let nextFree = getNextFree board position direction

  let nextResults = lift nextFree >>= expandPosition suffixTable dictionary newBoard newRack direction

  if Dict.contains currentMove dictionary && rackLength newRack <= 2
    then addResult (currentMove, position, direction) newDiscovered nextResults
    else nextResults

append :: a -> [a] -> [a]
append a as = a:as

addResult :: Result -> Discovered -> StateT Discovered [] Result -> StateT Discovered [] Result
addResult w d = mapStateT $ \x -> (w,d) : x
