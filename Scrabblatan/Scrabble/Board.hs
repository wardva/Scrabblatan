{-# LANGUAGE RecordWildCards #-}

module Scrabblatan.Scrabble.Board
  ( Board (..)
  , Bonus (..)
  , Cell (..)
  , Position
  , bonusCellUpdates
  , getCell
  , getBonus
  , getTile
  , emptyCell
  , index
  ) where

import           Data.Char                 (chr, ord)
import           Data.List                 (intercalate)
import           Data.Vector               ((!))
import qualified Data.Vector               as Vector

import           Scrabblatan.Scrabble.Tile
import           Scrabblatan.Scrabble.Bonus

type Position        = (Int, Int)


data Cell = Cell { bonus :: Maybe Bonus
                 , tile  :: Maybe Tile
                 }
  deriving (Eq, Show)

data Board = Board { boardSize :: Int
                   , getBoard  :: Vector.Vector Cell
                   }
  deriving (Eq)

index :: Int -> Position -> Int
index s (r, c) = r * s + c

emptyCell :: Cell
emptyCell = Cell { bonus = Nothing
                 , tile  = Nothing
                 }

bonusCellUpdates :: Int -> [Position] -> Bonus -> [(Int, Cell)]
bonusCellUpdates size ps b = fmap (\pos -> let idx = index size pos
                                               cell = emptyCell { bonus = Just b }
                                            in (idx, cell)
                                  ) ps

getCell :: Board -> Position -> Cell
getCell board pos = getBoard board ! index (boardSize board) pos

getBonus :: Board -> Position -> Maybe Bonus
getBonus board pos = bonus (getCell board pos)

getTile :: Board -> Position -> Maybe Tile
getTile board pos = tile (getCell board pos)

--Show instances

rowId :: Int -> String
rowId i = [chr (ord 'A' + i)]

colId :: Int -> String
colId i | i < 10 = " " ++ show i ++ "  "
        | otherwise = " " ++ show i ++ " "

instance Show Board where
  show Board {..} = colIds ++ border ++ raster
    where indices = [0 .. boardSize - 1]
          border = "---+"  ++ intercalate "+" (replicate boardSize "----") ++ "+\n"
          colIds = "\n   |" ++ intercalate "|" (colId <$> indices) ++ "|\n"
          showCell r c = let begin = if c == 0 then ' ' : rowId r ++ " |" else ""
                             end   = if c == boardSize - 1 then "|\n" ++ border else "|"
                             cell = getBoard ! index boardSize (r,c)
                             content = maybe "    " show (bonus cell)
                          in begin ++ content ++ end
          raster = concat [showCell r c | r <- indices, c <- indices]
