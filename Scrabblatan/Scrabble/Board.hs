{-# LANGUAGE RecordWildCards #-}

module Scrabblatan.Scrabble.Board
  ( Board (..)
  , Bonus (..)
  , Cell (..)
  , Direction (..)
  , Position
  , applyTiles
  , applyTile
  , directions
  , emptyCell
  , getTileRow
  , hasNeighbor
  , posToIndex
  , swapDirection
  , usablePositions
  , getNextFree
  ) where

import           Data.Char                  (chr, ord)
import           Data.List                  (intercalate)
import           Data.Maybe                 (isJust, isNothing, maybeToList)
import           Data.Vector                ((!), (//))
import qualified Data.Vector                as V

import           Scrabblatan.Scrabble.Bonus
import           Scrabblatan.Scrabble.Tile

-- Positioning

type BoardSize = Int
type Position  = (Int, Int)

rowId :: Int -> String
rowId i = [chr (ord 'A' + i)]

colId :: Int -> String
colId i | i < 9 = "  " ++ show (i+1) ++ "  "
        | otherwise = " " ++ show (i+1) ++ "  "

-- Directions

data Direction = Horizontal | Vertical
  deriving (Show, Eq)

directions :: [Direction]
directions = [Horizontal, Vertical]

swapDirection :: Direction -> Direction
swapDirection Horizontal = Vertical
swapDirection Vertical   = Horizontal

-- Cell

data Cell = Cell { bonus :: Maybe Bonus
                 , tile  :: Maybe Tile
                 }
  deriving (Eq, Show)

emptyCell :: Cell
emptyCell = Cell { bonus = Nothing
                 , tile  = Nothing
                 }

-- Board

data Board = Board { boardSize :: Int
                   , getBoard  :: V.Vector Cell
                   }
  deriving (Eq)

applyTiles :: Board -> [Maybe Tile] -> Board
applyTiles board tiles = let applied = V.zipWith (\c t -> c { tile = t }) (getBoard board) (V.fromList tiles)
                          in board { getBoard = applied }

applyTile :: Board -> Position -> Tile -> Board
applyTile board@Board {..} pos tile = let idx = posToIndex boardSize pos
                                          cell = (getBoard ! idx) { tile = Just tile }
                                       in board { getBoard = getBoard // [(idx, cell)] }

getTile :: Board -> Position -> Maybe Tile
getTile board pos = tile $ getBoard board ! posToIndex (boardSize board) pos

usablePositions :: Board -> [Position]
usablePositions board = filter usablePosition (boardPositions board)
  where usablePosition pos =
          isNothing (getTile board pos) && hasNeighbor board pos

hasNeighbor :: Board -> Position -> Bool
hasNeighbor board (r,c) = let neighbors = filter (valid board) [ (r+1,c), (r-1,c), (r,c+1), (r,c-1) ]
                           in or $ isJust . getTile board <$> neighbors

getTileRow :: Board -> Position -> Direction -> Tile -> [Tile]
getTileRow board pos dir tile =
  let (back, forward) = movements dir
      tilesBack = getDirection (back board) (back board pos)
      tilesForward = getDirection (forward board) (forward board pos)
   in reverse tilesBack ++ tile : tilesForward

  where getDirection next p =
          case p >>= getTile board of
            Just t  -> t : getDirection next (p >>= next)
            Nothing -> []

getNextFree :: Board -> Position -> Direction -> [Position]
getNextFree board pos dir =
  let (back, forward) = movements dir
   in getDirection (back board) (back board pos) ++ getDirection (forward board) (forward board pos)

  where getDirection next p = do
          p' <- maybeToList p
          case getTile board p' of
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
movements Vertical = (above, below)

--Show instances

instance Show Board where
  show Board {..} = colIds ++ border ++ raster
    where indices = [0 .. boardSize - 1]
          border = "-----+"  ++ intercalate "+" (replicate boardSize "-----") ++ "+\n"
          colIds = "\n     |" ++ intercalate "|" (colId <$> indices) ++ "|\n"
          showCell r c = let begin = if c == 0 then "  " ++ rowId r ++ "  |" else ""
                             end   = if c == boardSize - 1 then "|\n" ++ border else "|"
                             cell = getBoard ! posToIndex boardSize (r,c)
                             content = maybe (maybe "     " show (bonus cell)) show (tile cell)
                          in begin ++ content ++ end
          raster = concat [showCell r c | r <- indices, c <- indices]
