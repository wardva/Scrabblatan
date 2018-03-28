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

  describe "applyMove" $
    it "correctly applies two moves in the opposite direction" $ do
      let board = defaultBoard
      let move1 = [ ((7, 6),  Regular H)
                  , ((7, 7),  Regular E)
                  , ((7, 8),  Regular L)
                  , ((7, 9),  Regular L)
                  , ((7, 10), Regular O)
                  ]

      let move2 = [ ((6, 10),  Regular W)
                  , ((7, 10),  Regular O)
                  , ((8, 10),  Regular R)
                  , ((9, 10),  Regular L)
                  , ((10, 10), Regular D)
                  ]

      let firstApplied  = applyMove board move1
      let secondApplied = applyMove firstApplied move2

      let empty = Nothing
      let chr c = Just (Regular c)

 --                    1      2      3      4      5      6      7      8      9      10     11     12     13     14     15
{-A-} let tiles = [ empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
{-B-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
{-C-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
{-D-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
{-E-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
{-F-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
{-G-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr W, empty, empty, empty, empty
{-H-}             , empty, empty, empty, empty, empty, empty, chr H, chr E, chr L, chr L, chr O, empty, empty, empty, empty
{-I-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr R, empty, empty, empty, empty
{-J-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr L, empty, empty, empty, empty
{-K-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr D, empty, empty, empty, empty
{-L-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
{-M-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
{-N-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
{-O-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                  ]

      let expectedBoard  = applyTiles defaultBoard tiles

      secondApplied `shouldBe` expectedBoard
