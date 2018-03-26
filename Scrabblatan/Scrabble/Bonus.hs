module Scrabblatan.Scrabble.Bonus
  ( Bonus (..)
  , Multiplicator (..)
  , applyWordBonusses
  , applyCharBonusses
  ) where

data Multiplicator = Double | Triple
  deriving (Eq)

data Bonus = CharacterBonus { multiplicator :: Multiplicator }
           | WordBonus { multiplicator :: Multiplicator }
  deriving (Eq)

instance Show Multiplicator where
  show Double = "2"
  show Triple = "3"

instance Show Bonus where
  show (CharacterBonus m) = ' ' : show m ++ "C "
  show (WordBonus m)      = ' ' : show m ++ "W "

applyWordBonusses :: Int -> [Bonus] -> Int
applyWordBonusses = foldl applyWordBonus

applyCharBonusses :: [(Int, Maybe Bonus)] -> Int
applyCharBonusses = sum . fmap (\(s, b) -> maybe s (applyCharBonus s) b)

applyCharBonus :: Int -> Bonus -> Int
applyCharBonus score (CharacterBonus mul) = applyMultiplicator score mul
applyCharBonus score (WordBonus _)        = score

applyWordBonus :: Int -> Bonus -> Int
applyWordBonus score (WordBonus mul)    = applyMultiplicator score mul
applyWordBonus score (CharacterBonus _) = score

applyMultiplicator :: Int -> Multiplicator -> Int
applyMultiplicator score Double = score * 2
applyMultiplicator score Triple = score * 3
