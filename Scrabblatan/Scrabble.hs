{-# LANGUAGE RecordWildCards #-}

module Scrabblatan.Scrabble
  ( Board (..)
  , BoardSize
  , Bonus (..)
  , Cell (..)
  , Character (..)
  , Multiplicator (..)
  , Position
  , Tile (..)
  , bonusCellUpdates
  , emptyCell
  , index
  ) where

import qualified Data.Vector as Vector
import           Data.Vector ((!))
import           Data.Char (ord, chr)
import           Data.List (intercalate)

data Character = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Eq, Show)

data Multiplicator = Double | Triple
  deriving (Eq)


data Bonus = CharacterBonus { multiplicator :: Multiplicator }
           | WordBonus { multiplicator :: Multiplicator }
  deriving (Eq)

type BoardSize = Int
type Position = (Int, Int)

data Cell = Cell { bonus :: Maybe Bonus
                 , tile  :: Maybe Tile
                 }
  deriving (Eq, Show)

data Board = Board { size     :: BoardSize
                   , getBoard :: Vector.Vector Cell
                   }
  deriving (Eq)

data Tile = Tile { character :: Character
                 , value     :: Int
                 }
  deriving (Eq, Show)

index :: BoardSize -> Position -> Int
index s (r, c) = r * s + c

emptyCell :: Cell
emptyCell = Cell { bonus = Nothing
                 , tile  = Nothing
                 }

bonusCellUpdates :: BoardSize -> [Position] -> Bonus -> [(Int, Cell)]
bonusCellUpdates boardSize ps b = fmap (\pos -> let idx = index boardSize pos
                                                    cell = emptyCell { bonus = Just b }
                                                 in (idx, cell)
                                       ) ps

-- Show instances

rowId :: Int -> String
rowId i = [chr (ord 'A' + i)]

colId :: Int -> String
colId i | i < 10 = " " ++ show i ++ "  "
        | otherwise = " " ++ show i ++ " "

instance Show Multiplicator where
  show Double = "2"
  show Triple = "3"

instance Show Bonus where
  show (CharacterBonus m) = ' ' : show m ++ "C "
  show (WordBonus m)      = ' ' : show m ++ "W "

instance Show Board where
  show Board {..} = colIds ++ border ++ raster
    where indices = [0 .. size - 1]
          border = "---+"  ++ intercalate "+" (replicate size "----") ++ "+\n"
          colIds = "\n   |" ++ intercalate "|" (colId <$> indices) ++ "|\n"
          showCell r c = let begin = if c == 0 then ' ' : rowId r ++ " |" else ""
                             end   = if c == size - 1 then "|\n" ++ border else "|"
                             cell = getBoard ! index size (r,c)
                             content = maybe "    " show (bonus cell)
                          in begin ++ content ++ end
          raster = concat [showCell r c | r <- indices, c <- indices]
