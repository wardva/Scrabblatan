{-# LANGUAGE OverloadedStrings #-}

module Scrabblatan.Scrabble.ConfigSpec where

import           Control.Exception           (evaluate)
import qualified Data.Vector                 as Vector
import           Test.Hspec

import           Scrabblatan.Scrabble
import           Scrabblatan.Scrabble.Config

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "boardFromEnv" $ do
    it "correctly reads parameters and creates a board" $ do
      let env = [ ("SCRABBLATAN_DOUBLE_CHAR_POSITIONS", "[(1,1)]")
                , ("SCRABBLATAN_TRIPLE_CHAR_POSITIONS", "[]")
                , ("SCRABBLATAN_DOUBLE_WORD_POSITIONS", "[(0,0),(1,2)]")
                , ("SCRABBLATAN_TRIPLE_WORD_POSITIONS", "[(2,2),(0,1)]")
                , ("SCRABBLATAN_BOARD_SIZE", "5")
                ]

      let board = boardFromEnv' env

      let doubleChar = emptyCell { bonus = Just (CharacterBonus Double) }
      let doubleWord = emptyCell { bonus = Just (WordBonus Double) }
      let tripleWord = emptyCell { bonus = Just (WordBonus Triple) }

      board `shouldBe` Board { size = 5
                             , getBoard = Vector.fromList [ doubleWord, tripleWord, emptyCell,  tripleWord, doubleWord
                                                          , tripleWord, doubleChar, doubleWord, doubleChar, tripleWord
                                                          , emptyCell,  doubleWord, tripleWord, doubleWord, emptyCell
                                                          , tripleWord, doubleChar, doubleWord, doubleChar, tripleWord
                                                          , doubleWord, tripleWord, emptyCell,  tripleWord, doubleWord
                                                          ]
                             }


    it "correctly uses default parameter when parameter is not set" $ do
      let env = [("OTHER_PARAM", "123")]
      let board = boardFromEnv' env
      let vectorSize = Vector.length (getBoard board)

      size board `shouldBe` 15
      vectorSize `shouldBe` 225

    it "throws an error when parameter is not parsable" $ do
      let env = [("SCRABBLATAN_BOARD_SIZE", "abc")]
      let board = boardFromEnv' env

      evaluate (size board) `shouldThrow` anyErrorCall
