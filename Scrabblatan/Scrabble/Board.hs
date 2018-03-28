{-# LANGUAGE RecordWildCards #-}

module Scrabblatan.Scrabble.Board
  ( Board (..)
  , Bonus (..)
  , Cell (..)
  , Position
  , applyTiles
  , bonusCellUpdates
  , emptyCell
  , getBonus
  , hasNeighbor
  , posToIndex
  , usablePositions
  ) where

import           Data.Char                  (chr, ord)
import           Data.List                  (intercalate)
import           Data.Maybe                 (isJust, isNothing)
import           Data.Vector                ((!))
import qualified Data.Vector                as V

import           Scrabblatan.Scrabble.Bonus
import           Scrabblatan.Scrabble.Tile

type BoardSize = Int
type Position  = (Int, Int)


data Cell = Cell { bonus :: Maybe Bonus
                 , tile  :: Maybe Tile
                 }
  deriving (Eq, Show)

data Board = Board { boardSize :: Int
                   , getBoard  :: V.Vector Cell
                   }
  deriving (Eq)

applyTiles :: Board -> [Maybe Tile] -> Board
applyTiles board tiles = let applied = V.zipWith (\c t -> c { tile = t }) (getBoard board) (V.fromList tiles)
                          in board { getBoard = applied }

emptyCell :: Cell
emptyCell = Cell { bonus = Nothing
                 , tile  = Nothing
                 }

bonusCellUpdates :: BoardSize -> [Position] -> Bonus -> [(Int, Cell)]
bonusCellUpdates size ps b = fmap (\pos -> let idx = posToIndex size pos
                                               cell = emptyCell { bonus = Just b }
                                            in (idx, cell)
                                  ) ps

getCell :: Board -> Position -> Cell
getCell board pos = getBoard board ! posToIndex (boardSize board) pos

getBonus :: Board -> Position -> Maybe Bonus
getBonus board pos = bonus (getCell board pos)

getTile :: Board -> Position -> Maybe Tile
getTile board pos = tile (getCell board pos)

usablePositions :: Board -> [Position]
usablePositions board = filter (usablePosition board) (boardPositions board)


usablePosition :: Board -> Position -> Bool
usablePosition board pos = isNothing (getTile board pos) && hasNeighbor board pos

hasNeighbor :: Board -> Position -> Bool
hasNeighbor board (r,c) = let neighbors = filter (valid board) [ (r+1,c), (r-1,c), (r,c+1), (r,c-1) ]
                           in or $ isJust . getTile board <$> neighbors


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

--Show instances

rowId :: Int -> String
rowId i = [chr (ord 'A' + i)]

colId :: Int -> String
colId i | i < 9 = "  " ++ show (i+1) ++ "  "
        | otherwise = " " ++ show (i+1) ++ "  "

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
