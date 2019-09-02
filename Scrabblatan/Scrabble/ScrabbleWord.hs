{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabblatan.Scrabble.ScrabbleWord
  ( Character (..)
  , ScrabbleWord (..)
  , allCharacters
  , characterToChar
  , readPCharacter
  , wordLength
  ) where

import           Data.Char                    (toUpper)
import           Data.Functor                 (($>))
import           Data.Hashable                (Hashable)
import           GHC.Generics                 (Generic)
import           Text.ParserCombinators.ReadP

data Character = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Eq, Ord, Enum, Generic, Hashable)

instance Show Character where
  show c = [characterToChar c]

allCharacters :: [Character]
allCharacters = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z]

charMappings :: [(Char,Character)]
charMappings =
  [ ('a',A),('b',B),('c',C),('d',D),('e',E),('f',F),('g',G),('h',H),('i',I)
  , ('j',J),('k',K),('l',L),('m',M),('n',N),('o',O),('p',P),('q',Q),('r',R)
  , ('s',S),('t',T),('u',U),('v',V),('w',W),('x',X),('y',Y),('z',Z)
  ]

readPCharacter :: ReadP Character
readPCharacter =
  choice $ parse <$> mappings
  where
    parse (c, res) = char c +++ char (toUpper c) $> res
    mappings = charMappings

instance Read Character where
  readsPrec _  = readP_to_S readPCharacter

characterToChar :: Character -> Char
characterToChar A = 'a'
characterToChar B = 'b'
characterToChar C = 'c'
characterToChar D = 'd'
characterToChar E = 'e'
characterToChar F = 'f'
characterToChar G = 'g'
characterToChar H = 'h'
characterToChar I = 'i'
characterToChar J = 'j'
characterToChar K = 'k'
characterToChar L = 'l'
characterToChar M = 'm'
characterToChar N = 'n'
characterToChar O = 'o'
characterToChar P = 'p'
characterToChar Q = 'q'
characterToChar R = 'r'
characterToChar S = 's'
characterToChar T = 't'
characterToChar U = 'u'
characterToChar V = 'v'
characterToChar W = 'w'
characterToChar X = 'x'
characterToChar Y = 'y'
characterToChar Z = 'z'

newtype ScrabbleWord = ScrabbleWord [Character]
  deriving (Eq, Ord)
  deriving newtype (Hashable, NFData)

wordLength :: ScrabbleWord -> Int
wordLength (ScrabbleWord w) = length w

instance Show ScrabbleWord where
  show (ScrabbleWord chars) = concatMap show chars

readPScrabbleWord :: ReadP ScrabbleWord
readPScrabbleWord = ScrabbleWord <$> many readPCharacter

instance Read ScrabbleWord where
  readsPrec _ = readP_to_S readPScrabbleWord
