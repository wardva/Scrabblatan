{-# LANGUAGE OverloadedStrings #-}

module Scrabblatan.Scrabble.BoardSpec where

import           Test.Hspec
import qualified Data.Vector as V

import           Scrabblatan.Scrabble
import           Scrabblatan.Scrabble.Config

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hasNeighbor" $ do
    let empty = Cell { bonus = Nothing, tile = Nothing }
    let chr c = Cell { bonus = Nothing, tile = Just (Regular c) }

    let tiles = [ empty, empty, empty, empty, empty, empty, empty, empty
                , empty, empty, chr R, empty, chr B, empty, empty, empty
                , chr S, chr N, chr E, chr D, chr E, chr N, empty, empty
                , empty, empty, chr D, empty, chr E, empty, empty, empty
                , empty, empty, chr E, chr N, chr K, chr E, chr L, chr E
                , empty, empty, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, empty, empty
                ]

    let board  = Board { boardSize = 8, getBoard = V.fromList tiles }

    it "returns False when no occupied positions next to position" $ do
      hasNeighbor board (0, 0) `shouldBe` False
      hasNeighbor board (7, 7) `shouldBe` False
      hasNeighbor board (6, 2) `shouldBe` False
      hasNeighbor board (5, 1) `shouldBe` False

    it "returns True when occupied positions next to position" $ do
      hasNeighbor board (1, 0) `shouldBe` True
      hasNeighbor board (2, 0) `shouldBe` True
      hasNeighbor board (3, 3) `shouldBe` True

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

  describe "getTileRow" $ do
    let empty = Nothing
    let chr c = Just (Regular c)

    let tiles = [ empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, chr R, empty, chr B, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, chr S, chr N, chr E, chr D, chr E, chr N, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, chr D, empty, chr E, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, chr E, chr N, chr K, chr E, chr L, chr E, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr N, chr A, chr F, chr T
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr O
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr E
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr T
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr S
                ]

    let board = applyTiles defaultBoard tiles

    it "returns the new TileRow after putting a Tile in vertical direction" $ do
      let tiles = getTileRow board (8, 7) Horizontal (Regular O)

      tiles `shouldBe` [Regular D, Regular O, Regular E]

    it "returns the new TileRow after putting a Tile in vertical direction" $ do
      let tiles = getTileRow board (8, 7) Vertical (Regular O)

      tiles `shouldBe` [Regular D, Regular O, Regular N]

    it "detects Board borders" $ do
      let tiles = getTileRow board (14, 13) Horizontal (Regular O)

      tiles `shouldBe` [Regular O, Regular S]

  describe "getNextFree" $ do
    let empty = Nothing
    let chr c = Just (Regular c)

    let tiles = [ empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, chr R, empty, chr B, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, chr S, chr N, chr E, chr D, chr E, chr N, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, chr D, empty, chr E, empty, empty, empty, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, chr E, chr N, chr K, chr E, chr L, chr E, empty, empty, empty
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr N, chr A, chr F, chr T
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr O
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr E
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr T
                , empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, chr S
                ]

    let board = applyTiles defaultBoard tiles

    it "returns the next free horizontal tiles" $ do
      let nextFree = getNextFree board (7, 6) Horizontal

      nextFree `shouldBe` [(7, 3), (7, 10)]
