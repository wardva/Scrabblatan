{-# LANGUAGE OverloadedStrings #-}

module Scrabblatan.Scrabble.TileSpec where

import           Control.Exception         (evaluate)
import           Test.Hspec

import           Scrabblatan.Scrabble

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Read instance" $ do
    it "reads Blanco tile" $ do
      let str = "_"
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
