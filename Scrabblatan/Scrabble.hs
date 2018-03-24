{-# LANGUAGE RecordWildCards #-}

module Scrabblatan.Scrabble
  ( Board (..)
  , Bonus (..)
  , Cell (..)
  , CharacterValue
  , CharacterValues
  , Multiplicator (..)
  , Position
  , Character (..)
  , Rack (..)
  , bonusCellUpdates
  , emptyCell
  , index
  ) where

import           Data.Char       (chr, ord)
import           Data.List       (intercalate)
import qualified Data.Map.Strict as Map
import           Data.Vector     ((!))
import qualified Data.Vector     as Vector

data Character = Blanco | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Eq, Ord, Read, Show)

data Multiplicator = Double | Triple
  deriving (Eq)

data Bonus = CharacterBonus { multiplicator :: Multiplicator }
           | WordBonus { multiplicator :: Multiplicator }
  deriving (Eq)

type CharacterValue  = Int
type CharacterValues = Map.Map Character CharacterValue
type Position        = (Int, Int)

data Cell = Cell { bonus :: Maybe Bonus
                 , char  :: Maybe Character
                 }
  deriving (Eq, Show)

data Board = Board { boardSize :: Int
                   , getBoard  :: Vector.Vector Cell
                   }
  deriving (Eq)

data Rack = Rack { rackSize :: Int
                 , tiles    :: [ Character ]
                 }
  deriving (Eq, Show)

index :: Int -> Position -> Int
index s (r, c) = r * s + c

emptyCell :: Cell
emptyCell = Cell { bonus = Nothing
                 , char  = Nothing
                 }

bonusCellUpdates :: Int -> [Position] -> Bonus -> [(Int, Cell)]
bonusCellUpdates size ps b = fmap (\pos -> let idx = index size pos
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
    where indices = [0 .. boardSize - 1]
          border = "---+"  ++ intercalate "+" (replicate boardSize "----") ++ "+\n"
          colIds = "\n   |" ++ intercalate "|" (colId <$> indices) ++ "|\n"
          showCell r c = let begin = if c == 0 then ' ' : rowId r ++ " |" else ""
                             end   = if c == boardSize - 1 then "|\n" ++ border else "|"
                             cell = getBoard ! index boardSize (r,c)
                             content = maybe "    " show (bonus cell)
                          in begin ++ content ++ end
          raster = concat [showCell r c | r <- indices, c <- indices]
