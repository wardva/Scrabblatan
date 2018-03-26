{-# LANGUAGE OverloadedStrings #-}

module Scrabblatan.Scrabble.TileSpec where

import           Control.Exception         (evaluate)
import           Test.Hspec

import           Scrabblatan.Scrabble.Tile

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Read instance" $ do
    it "reads Blanco tile" $ do
      let str = "Blanco"
      read str `shouldBe` Blanco

    it "reads Regular tiles from uppercase" $ do
      read "A" `shouldBe` Regular A
      read "W" `shouldBe` Regular W
      read "Z" `shouldBe` Regular Z

    it "reads Regular tiles from lowercase" $ do
      read "a" `shouldBe` Regular A
      read "w" `shouldBe` Regular W
      read "z" `shouldBe` Regular Z

    it "throws an error when invalid character" $
      evaluate (read "%" :: Tile) `shouldThrow` anyErrorCall

    it "throws an error when multiple characters" $
      evaluate (read "AB" :: Tile) `shouldThrow` anyErrorCall

  describe "matches" $ do
    it "character matches a tile" $ do
      let char = A
      let tile = Regular A

      matches char tile `shouldBe` True

    it "character doesn't match a tile" $ do
      let char = B
      let tile = Regular A

      matches char tile `shouldBe` False

    it "any character matches Blanco" $ do
      let char1 = A
      let char2 = B
      let tile = Blanco

      matches char1 tile `shouldBe` True
      matches char2 tile `shouldBe` True
