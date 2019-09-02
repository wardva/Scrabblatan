{-# LANGUAGE OverloadedStrings #-}

module Scrabblatan.Scrabble.BoardSpec where

import           Test.Hspec
import qualified Data.Vector as V

import           Scrabblatan.Scrabble

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let e = Nothing
  let chr c = Just c

  describe "hasNeighbor" $ do
    let board =  read $ ". . . . . . . .\n" ++
                        ". . R . B . . .\n" ++
                        "S N E D E N . .\n" ++
                        ". . D . E . . .\n" ++
                        ". . E N K E L E\n" ++
                        ". . . . . . . .\n" ++
                        ". . . . . . . .\n" ++
                        ". . . . . . . .\n"

    it "returns False when no occupied positions next to position" $ do
      hasNeighbor board (0, 0) `shouldBe` False
      hasNeighbor board (7, 7) `shouldBe` False
      hasNeighbor board (6, 2) `shouldBe` False
      hasNeighbor board (5, 1) `shouldBe` False

    it "returns True when occupied positions next to position" $ do
      hasNeighbor board (1, 0) `shouldBe` True
      hasNeighbor board (2, 0) `shouldBe` True
      hasNeighbor board (3, 3) `shouldBe` True

  let board = read $ ". . . . . . . . . . . . . . .\n" ++
                     ". . . . . . . . . . . . . . .\n" ++
                     ". . . . . . . . . . . . . . .\n" ++
                     ". . . . . . . . . . . . . . .\n" ++
                     ". . . . . . . . . . . . . . .\n" ++
                     ". . . . . . . . . . . . . . .\n" ++
                     ". . . . . . R . B . . . . . .\n" ++
                     ". . . . S N E D E N . . . . .\n" ++
                     ". . . . . . D . E . . . . . .\n" ++
                     ". . . . . . E N K E L E . . .\n" ++
                     ". . . . . . . . . . . N A F T\n" ++
                     ". . . . . . . . . . . . . . O\n" ++
                     ". . . . . . . . . . . . . . E\n" ++
                     ". . . . . . . . . . . . . . T\n" ++
                     ". . . . . . . . . . . . . . S\n"

  describe "usablePostions" $
    it "returns all usable positions (empty cells with occupied neighbors)" $ do
      let expected = [ (5, 6),  (5, 8),   (6, 4),   (6, 5),   (6, 7),   (6, 9),   (7, 3)
                     , (7, 10), (8, 4),   (8, 5),   (8, 7),   (8, 9),   (8, 10),  (8, 11)
                     , (9, 5),  (9, 12),  (9, 13),  (9, 14),  (10, 6),  (10, 7),  (10, 8)
                     , (10, 9), (10, 10), (11, 11), (11, 12), (11, 13), (12, 13), (13, 13)
                     , (14, 13)
                     ]

      let usable = usablePositions board

      length usable `shouldBe` 29
      usable `shouldBe` expected

  describe "getRow" $ do
    it "returns new row after putting a Character in vertical direction" $ do
      let characters = getRow board (8, 7) Horizontal O

      characters `shouldBe` ScrabbleWord [D, O, E]

    it "returns new Row after putting a Character in vertical direction" $ do
      let characters = getRow board (8, 7) Vertical O

      characters `shouldBe` ScrabbleWord [D, O, N]

    it "detects Board borders" $ do
      let characters = getRow board (14, 13) Horizontal O

      characters `shouldBe` ScrabbleWord [O, S]

  describe "getNextFree" $
    it "returns the next free horizontal characters" $ do
      let nextFree = getNextFree board (7, 6) Horizontal

      nextFree `shouldBe` [(7, 3), (7, 10)]

  describe "Show and Read instance" $ do
    let boardStr = ". . . . . . . . . . . . . . .\n" ++
                   ". . . . . . . . . . . . . . .\n" ++
                   ". . . . . . . . . . . . . . .\n" ++
                   ". . . . . . . . . . . . . . .\n" ++
                   ". . . . . . . . . . . . . . .\n" ++
                   ". . . . . . . . . . . . . . .\n" ++
                   ". . . . . . r . b . . . . . .\n" ++
                   ". . . . s n e d e n . . . . .\n" ++
                   ". . . . . . d . e . . . . . .\n" ++
                   ". . . . . . e n k e l e . . .\n" ++
                   ". . . . . . . . . . . n a f t\n" ++
                   ". . . . . . . . . . . . . . o\n" ++
                   ". . . . . . . . . . . . . . e\n" ++
                   ". . . . . . . . . . . . . . t\n" ++
                   ". . . . . . . . . . . . . . s\n"

    it "shows a board correctly" $
      show board `shouldBe` boardStr

    it "reads a board correctly" $
      read boardStr `shouldBe` board
