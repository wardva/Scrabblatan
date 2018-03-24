{-# LANGUAGE OverloadedStrings #-}

module Scrabblatan.Scrabble.ConfigSpec where

import           Control.Exception           (evaluate)
import qualified Data.Map.Strict             as Map
import qualified Data.Vector                 as Vector
import           Test.Hspec

import           Scrabblatan.Scrabble
import           Scrabblatan.Scrabble.Config

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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

      board `shouldBe` Board { boardSize = 5
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

      boardSize board `shouldBe` 15
      vectorSize `shouldBe` 225

    it "throws an error when parameter is not parsable" $ do
      let env = [("SCRABBLATAN_BOARD_SIZE", "abc")]
      let board = boardFromEnv' env

      evaluate (boardSize board) `shouldThrow` anyErrorCall



  describe "boardFromEnv" $ do
    it "correctly reads parameters and creates a board" $ do
      let env = [ ("SCRABBLATAN_CHARACTER_VALUES", "[(A,1),(B,2),(C,3),(D,4),(E,5),(F,6),(G,7),(H,8),(I,9),(J,10),(K,11),(L,12),(M,13),(N,14),(O,15),(P,16),(Q,17),(R,18),(S,19),(T,20),(U,21),(V,22),(W,23),(X,24),(Y,25),(Z,26),(Blanco,0)]") ]
      let characterValues = characterValuesFromEnv' env

      characterValues `shouldBe` Map.fromList [ (A, 1),  (B, 2),  (C, 3),  (D, 4),  (E, 5),  (F, 6)
                                              , (G, 7),  (H, 8),  (I, 9),  (J, 10), (K, 11), (L, 12)
                                              , (M, 13), (N, 14), (O, 15), (P, 16), (Q, 17), (R, 18)
                                              , (S, 19), (T, 20), (U, 21), (V, 22), (W, 23), (X, 24)
                                              , (Y, 25), (Z, 26), (Blanco, 0)
                                              ]

    it "correctly uses default parameters when parameter is not set" $ do
      let env = [ ]
      let characterValues = characterValuesFromEnv' env

      characterValues `shouldBe` Map.fromList [ (A, 1), (B, 3), (C, 5), (D, 2), (E, 1),  (F, 4)
                                              , (G, 3), (H, 4), (I, 1), (J, 4), (K, 3),  (L, 3)
                                              , (M, 3), (N, 1), (O, 1), (P, 3), (Q, 10), (S, 2)
                                              , (T, 2), (U, 4), (V, 4), (W, 5), (X, 8),  (Y, 8)
                                              , (Z, 4), (Blanco, 0)
                                              ]

    it "throws an error when parameter is not parsable" $ do
      let env = [("SCRABBLATAN_CHARACTER_VALUES", "abc")]
      let characterValues = characterValuesFromEnv' env

      evaluate characterValues `shouldThrow` anyErrorCall
