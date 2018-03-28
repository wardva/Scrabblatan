{-# LANGUAGE OverloadedStrings #-}

module Scrabblatan.Scrabble.MoveSpec where

import           Test.Hspec

import           Scrabblatan.Scrabble
import           Scrabblatan.Scrabble.Config

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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

  describe "buildMove" $ do
    it "correctly build a horizontal move" $ do
      let tiles = [Regular H, Regular E, Regular L, Regular L, Regular O]
      let direction = Horizontal
      let startPosition = (2, 3)

      let expected = [ ((2, 3), Regular H)
                     , ((2, 4), Regular E)
                     , ((2, 5), Regular L)
                     , ((2, 6), Regular L)
                     , ((2, 7), Regular O)
                     ]

      buildMove tiles direction startPosition `shouldBe` expected

    it "correctly build a vertical move" $ do
      let tiles = [Regular H, Regular E, Regular L, Regular L, Regular O]
      let direction = Vertical
      let startPosition = (2, 3)

      let expected = [ ((2, 3), Regular H)
                     , ((3, 3), Regular E)
                     , ((4, 3), Regular L)
                     , ((5, 3), Regular L)
                     , ((6, 3), Regular O)
                     ]

      buildMove tiles direction startPosition `shouldBe` expected

  describe "applyMove" $
    it "correctly applies two moves in the opposite direction" $ do
      let move1 = buildMove [Regular H, Regular E, Regular L, Regular L, Regular O] Horizontal (7, 6)
      let move2 = buildMove [Regular W, Regular O, Regular R, Regular L, Regular D] Vertical (6, 10)

      let firstApplied  = applyMove defaultBoard move1
      let secondApplied = applyMove firstApplied move2

      let empty = Nothing
      let chr c = Just (Regular c)

      let tiles = [ empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                  , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                  , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                  , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                  , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                  , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                  , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr W, empty, empty, empty, empty
                  , empty, empty, empty, empty, empty, empty, chr H, chr E, chr L, chr L, chr O, empty, empty, empty, empty
                  , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr R, empty, empty, empty, empty
                  , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr L, empty, empty, empty, empty
                  , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr D, empty, empty, empty, empty
                  , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                  , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                  , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                  , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                  ]

      let expectedBoard  = applyTiles defaultBoard tiles

      secondApplied `shouldBe` expectedBoard
