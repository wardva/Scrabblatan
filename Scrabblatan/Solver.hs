module Scrabblatan.Solver where

import           Control.Monad            (guard, when)
import           Control.Monad.State.Lazy
import qualified Data.HashSet             as Set

import qualified Scrabblatan.Dictionary   as Dict
import           Scrabblatan.Scrabble
import qualified Scrabblatan.SuffixMap    as Suff

type Result = (ScrabbleWord, [Position], Direction)

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

  let additional = snd $ getRow board position (swapDirection direction) currentCharacter

  when (wordLength additional > 1) $
    guard $ additional `Dict.contains` dictionary

  let (wordPositions, word) = getRow board position direction currentCharacter
  let discovery = (word, wordPositions, direction)

  discovered <- get

  guard $ not (Set.member discovery discovered)

  put $ Set.insert discovery discovered

  guard $ Suff.contains word suffixTable

  let newRack = remove rack rackTile
  let newBoard = applyCharacter board position currentCharacter
  let nextFree = getNextFree board position direction

  let nextResults = lift nextFree >>= expandPosition suffixTable dictionary newBoard newRack direction

  if Dict.contains word dictionary
    then return (word, wordPositions, direction)
    else nextResults

append :: a -> [a] -> [a]
append a as = a:as
