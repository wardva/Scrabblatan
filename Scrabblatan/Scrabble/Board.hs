{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Scrabblatan.Scrabble.Board
  ( Board (..)
  , Bonus (..)
  , Direction (..)
  , Position
  , fillBoard
  , applyCharacter
  , readPBoard
  , directions
  , getRow
  , hasNeighbor
  , posToIndex
  , swapDirection
  , usablePositions
  , getNextFree
  ) where

import           Data.Functor                 (($>))
import           Data.Hashable                (Hashable)
import qualified Data.HashMap.Lazy            as Map
import           Data.Maybe                   (isJust, isNothing, maybeToList)
import           Data.Vector                  ((!), (//))
import qualified Data.Vector                  as V
import           GHC.Generics                 (Generic)
import           Text.ParserCombinators.ReadP


import           Scrabblatan.Scrabble.Bonus
import           Scrabblatan.Scrabble.ScrabbleWord

-- Positioning

type BoardSize = Int
type Position  = (Int, Int)
type Bonuses   = Map.HashMap Position Bonus

-- Directions

data Direction = Horizontal | Vertical
  deriving (Show, Eq, Generic, Hashable)

directions :: [Direction]
directions = [Horizontal, Vertical]

swapDirection :: Direction -> Direction
swapDirection Horizontal = Vertical
swapDirection Vertical   = Horizontal

-- Board

data Board = Board { boardSize  :: !BoardSize
                   , getBoard   :: !(V.Vector (Maybe Character))
                   , getBonuses :: !Bonuses
                   }
  deriving (Eq)

fillBoard :: Board -> [Maybe Character] -> Board
fillBoard board tiles = board { getBoard = V.fromList tiles }

applyCharacter:: Board -> Position -> Character -> Board
applyCharacter board@Board {..} pos tile = let idx = posToIndex boardSize pos
                                       in board { getBoard = getBoard // [(idx, Just tile)] }

getCharacter :: Board -> Position -> Maybe Character
getCharacter board pos = getBoard board ! posToIndex (boardSize board) pos

usablePositions :: Board -> [Position]
usablePositions board = filter usablePosition (boardPositions board)
  where usablePosition pos =
          isNothing (getCharacter board pos) && hasNeighbor board pos

hasNeighbor :: Board -> Position -> Bool
hasNeighbor board (r,c) = let neighbors = filter (valid board) [ (r+1,c), (r-1,c), (r,c+1), (r,c-1) ]
                           in or $ isJust . getCharacter board <$> neighbors

getRow :: Board -> Position -> Direction -> Character -> ScrabbleWord
getRow board pos dir tile =
  let (back, forward) = movements dir
      tilesBack = getDirection (back board) (back board pos)
      tilesForward = getDirection (forward board) (forward board pos)
   in ScrabbleWord (reverse tilesBack ++ tile : tilesForward)

  where getDirection next p =
          case p >>= getCharacter board of
            Just t  -> t : getDirection next (p >>= next)
            Nothing -> []

getNextFree :: Board -> Position -> Direction -> [Position]
getNextFree board pos dir =
  let (back, forward) = movements dir
   in getDirection (back board) (back board pos) ++ getDirection (forward board) (forward board pos)

  where getDirection next p = do
          p' <- maybeToList p
          case getCharacter board p' of
            Just _  -> getDirection next (next p')
            Nothing -> return p'

--Helper functions

posToIndex :: BoardSize -> Position -> Int
posToIndex size (r, c) = r * size + c

indexToPos :: BoardSize -> Int -> Position
indexToPos size idx = (idx `div` size, idx `mod` size)

boardIndexes :: Board -> [Int]
boardIndexes board = let n = boardSize board
                      in [0 .. (n * n) - 1]

boardPositions :: Board -> [Position]
boardPositions board = let n = boardSize board
                        in indexToPos n <$> boardIndexes board

valid :: Board -> Position -> Bool
valid board (r, c) = let n = boardSize board
                      in r >= 0 && r < n && c >= 0 && c < n

validate :: Position -> Board -> Maybe Position
validate pos board
  | valid board pos = Just pos
  | otherwise = Nothing

type Movement = Board -> Position -> Maybe Position

leftOf, rightOf, above, below :: Movement
leftOf b (r, c)  = validate (r, c-1) b
rightOf b (r, c) = validate (r, c+1) b
above b (r, c)   = validate (r-1, c) b
below b (r, c)   = validate (r+1, c) b

movements :: Direction -> (Movement, Movement)
movements Horizontal = (leftOf, rightOf)
movements Vertical   = (above, below)

--Show and Read instances

instance Show Board where
  show Board {..} =
    V.ifoldr (\i e acc ->
      let c = case e of
                Just c'  -> characterToChar c'
                Nothing -> '.'
       in c : (if (i + 1) `mod` 15 == 0 then '\n' else ' ') : acc
    ) mempty getBoard

readPBoard :: ReadP Board
readPBoard = do
  firstLine <- skipSpaces *> readLine
  let n = length firstLine
  otherLines <- count (n - 1) readLine
  let characters = firstLine ++ concat otherLines

  return Board { boardSize = n
               , getBoard = V.fromList characters
               , getBonuses = mempty
               }

  where readLine = manyTill readCharacter (char '\n') <* skipSpaces
        readCharacter = ((char '.' $> Nothing) +++ (Just <$> readPCharacter)) <* many (char ' ')

instance Read Board where
  readsPrec _ = readP_to_S readPBoard
