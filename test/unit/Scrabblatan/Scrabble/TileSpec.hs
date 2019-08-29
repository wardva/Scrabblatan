{-# LANGUAGE OverloadedStrings #-}

module Scrabblatan.Scrabble.TileSpec where

import           Control.Exception         (evaluate)
import           Test.Hspec

import           Scrabblatan.Scrabble

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

  describe "expandBlanco" $ do
    it "returns all characters" $ do
      let tiles = [Regular H, Regular E, Regular L, Regular L, Regular O]
      let possibilities = expandBlanco tiles

      possibilities `shouldBe` [ScrabbleWord [H,E,L,L,O]]

    it "expands a blanco tile" $ do
      let tiles = [Regular H, Blanco, Regular L, Regular L, Regular O]
      let possibilities = expandBlanco tiles

      possibilities `shouldBe`
        ScrabbleWord <$>
          [ [H,A,L,L,O],[H,B,L,L,O],[H,C,L,L,O],[H,D,L,L,O]
          , [H,E,L,L,O],[H,F,L,L,O],[H,G,L,L,O],[H,H,L,L,O]
          , [H,I,L,L,O],[H,J,L,L,O],[H,K,L,L,O],[H,L,L,L,O]
          , [H,M,L,L,O],[H,N,L,L,O],[H,O,L,L,O],[H,P,L,L,O]
          , [H,Q,L,L,O],[H,R,L,L,O],[H,S,L,L,O],[H,T,L,L,O]
          , [H,U,L,L,O],[H,V,L,L,O],[H,W,L,L,O],[H,X,L,L,O]
          , [H,Y,L,L,O],[H,Z,L,L,O]
          ]
