{-# LANGUAGE OverloadedStrings #-}

module Scrabblatan.Scrabble.BoardSpec where

import           Control.Exception           (evaluate)
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

  describe "usablePostions" $
    it "returns all usable positions (empty cells with occupied neighbors)" $ do
      let empty = Nothing
      let chr c = Just (Regular c)

 --                    1      2      3      4      5      6      7      8      9      10     11     12     13     14     15
{-A-} let tiles = [ empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
{-B-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
{-C-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
{-D-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
{-E-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
{-F-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
{-G-}             , empty, empty, empty, empty, empty, empty, chr R, empty, chr B, empty, empty, empty, empty, empty, empty
{-H-}             , empty, empty, empty, empty, chr S, chr N, chr E, chr D, chr E, chr N, empty, empty, empty, empty, empty
{-I-}             , empty, empty, empty, empty, empty, empty, chr D, empty, chr E, empty, empty, empty, empty, empty, empty
{-J-}             , empty, empty, empty, empty, empty, empty, chr E, chr N, chr K, chr E, chr L, chr E, empty, empty, empty
{-K-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr N, chr A, chr F, chr T
{-L-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr O
{-M-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr E
{-N-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr T
{-O-}             , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr S
                  ]

      let board  = applyTiles defaultBoard tiles
      let expected = [ (5, 6),  (5, 8),   (6, 4),   (6, 5),   (6, 7),   (6, 9),   (7, 3)
                     , (7, 10), (8, 4),   (8, 5),   (8, 7),   (8, 9),   (8, 10),  (8, 11)
                     , (9, 5),  (9, 12),  (9, 13),  (9, 14),  (10, 6),  (10, 7),  (10, 8)
                     , (10, 9), (10, 10), (11, 11), (11, 12), (11, 13), (12, 13), (13, 13)
                     , (14, 13)
                     ]

      let usable = usablePositions board

      length usable `shouldBe` 29
      usable `shouldBe` expected

