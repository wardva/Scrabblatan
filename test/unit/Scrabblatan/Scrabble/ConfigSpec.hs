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
      let env = [ ("SCRABBLATAN_TILE_VALUES", "[(A,1),(B,2),(C,3),(D,4),(E,5),(F,6),(G,7),(H,8),(I,9),(J,10),(K,11),(L,12),(M,13),(N,14),(O,15),(P,16),(Q,17),(R,18),(S,19),(T,20),(U,21),(V,22),(W,23),(X,24),(Y,25),(Z,26),(Blanco,0)]") ]
      let tileValues = tileValuesFromEnv' env

      tileValues `shouldBe` [ (Regular A, 1),  (Regular B, 2),  (Regular C, 3),  (Regular D, 4),  (Regular E, 5),  (Regular F, 6)
                            , (Regular G, 7),  (Regular H, 8),  (Regular I, 9),  (Regular J, 10), (Regular K, 11), (Regular L, 12)
                            , (Regular M, 13), (Regular N, 14), (Regular O, 15), (Regular P, 16), (Regular Q, 17), (Regular R, 18)
                            , (Regular S, 19), (Regular T, 20), (Regular U, 21), (Regular V, 22), (Regular W, 23), (Regular X, 24)
                            , (Regular Y, 25), (Regular Z, 26), (Blanco, 0)
                            ]

    it "correctly uses default parameters when parameter is not set" $ do
      let env = [ ]
      let tileValues = tileValuesFromEnv' env

      tileValues `shouldBe` [ (Regular A, 1), (Regular B, 3), (Regular C, 5), (Regular D, 2), (Regular E, 1),  (Regular F, 4)
                            , (Regular G, 3), (Regular H, 4), (Regular I, 1), (Regular J, 4), (Regular K, 3),  (Regular L, 3)
                            , (Regular M, 3), (Regular N, 1), (Regular O, 1), (Regular P, 3), (Regular Q, 10), (Regular S, 2)
                            , (Regular T, 2), (Regular U, 4), (Regular V, 4), (Regular W, 5), (Regular X, 8),  (Regular Y, 8)
                            , (Regular Z, 4), (Blanco, 0)
                            ]

    it "throws an error when parameter is not parsable" $ do
      let env = [("SCRABBLATAN_TILE_VALUES", "abc")]
      let tileValues = tileValuesFromEnv' env

      evaluate tileValues `shouldThrow` anyErrorCall
