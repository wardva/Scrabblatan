{-# LANGUAGE OverloadedStrings #-}

module Scrabblatan.Scrabble.MoveSpec where

import           Test.Hspec

import           Scrabblatan.Scrabble
import           Scrabblatan.Scrabble.Config

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "score" $ do
    let board = defaultBoard
    let values = defaultTileValues

    it "correctly calculates the score (2 x char and two times 3 x word bonus)" $ do
      let move = [ ((14, 0), Regular Q)
                 , ((14, 1), Regular U)
                 , ((14, 2), Regular O)
                 , ((14, 3), Regular T)
                 , ((14, 4), Regular I)
                 , ((14, 5), Regular E)
                 , ((14, 6), Regular N)
                 , ((14, 7), Blanco)
                 ]

      let result = 198
      score board values move `shouldBe` result

    it "correctly calculates the score of two blanco tiles" $ do
      let move = [ ((7, 7), Blanco)
                 , ((7, 8), Blanco)
                 ]

      let result = 0
      score board values move `shouldBe` result

    it "correctly calculates the score (3 x char and 2 x word bonus)" $ do
      let move = [ ((1, 13), Regular W)
                 , ((2, 13), Regular O)
                 , ((3, 13), Regular E)
                 , ((4, 13), Regular S)
                 , ((5, 13), Regular T)
                 ]

      let result = 30
      score board values move `shouldBe` result
