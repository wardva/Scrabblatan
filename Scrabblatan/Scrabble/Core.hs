{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabblatan.Scrabble.Core
  ( Character (..)
  , ScrabbleWord (..)
  ) where

import           Data.Hashable (Hashable)
import           GHC.Generics  (Generic)
import           Data.Char     (toLower)

data Character = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Eq, Ord, Enum, Generic, Hashable)

instance Show Character where
  show A = "a"
  show B = "b"
  show C = "c"
  show D = "d"
  show E = "e"
  show F = "f"
  show G = "g"
  show H = "h"
  show I = "i"
  show J = "j"
  show K = "k"
  show L = "l"
  show M = "m"
  show N = "n"
  show O = "o"
  show P = "p"
  show Q = "q"
  show R = "r"
  show S = "s"
  show T = "t"
  show U = "u"
  show V = "v"
  show W = "w"
  show X = "x"
  show Y = "y"
  show Z = "z"

instance Read Character where
  readsPrec _ (c:cs) =
    case toLower c of
      'a' -> [(A, cs)]
      'b' -> [(B, cs)]
      'c' -> [(C, cs)]
      'd' -> [(D, cs)]
      'e' -> [(E, cs)]
      'f' -> [(F, cs)]
      'g' -> [(G, cs)]
      'h' -> [(H, cs)]
      'i' -> [(I, cs)]
      'j' -> [(J, cs)]
      'k' -> [(K, cs)]
      'l' -> [(L, cs)]
      'm' -> [(M, cs)]
      'n' -> [(N, cs)]
      'o' -> [(O, cs)]
      'p' -> [(P, cs)]
      'q' -> [(Q, cs)]
      'r' -> [(R, cs)]
      's' -> [(S, cs)]
      't' -> [(T, cs)]
      'u' -> [(U, cs)]
      'v' -> [(V, cs)]
      'w' -> [(W, cs)]
      'x' -> [(X, cs)]
      'y' -> [(Y, cs)]
      'z' -> [(Z, cs)]
      _   -> []
  readsPrec _ _ = []

newtype ScrabbleWord = ScrabbleWord [Character]
  deriving (Eq, Ord)
  deriving newtype (Hashable)

instance Show ScrabbleWord where
  show (ScrabbleWord chars) = concatMap show chars

instance Read ScrabbleWord where
  readsPrec _ "" = [(ScrabbleWord [], "")]
  readsPrec n str = do
    (c, other) <- readsPrec n str

    case readsPrec n other of
      (ScrabbleWord cs, other'):_ -> [(ScrabbleWord (c:cs), other')]
      []                          -> [(ScrabbleWord [c], other)]
