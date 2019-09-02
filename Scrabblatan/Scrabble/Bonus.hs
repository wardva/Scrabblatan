module Scrabblatan.Scrabble.Bonus
  ( Bonus (..)
  , Multiplicator (..)
  , applyWordBonusses
  , applyCharBonusses
  ) where

data Multiplicator = Double | Triple
  deriving (Eq, Show)

data Bonus = CharacterBonus { multiplicator :: Multiplicator }
           | WordBonus { multiplicator :: Multiplicator }
  deriving (Eq, Show)

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
