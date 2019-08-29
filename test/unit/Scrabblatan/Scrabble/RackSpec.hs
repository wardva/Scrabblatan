{-# LANGUAGE OverloadedStrings #-}

module Scrabblatan.Scrabble.RackSpec where

import           Test.Hspec

import           Scrabblatan.Scrabble

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "remove" $
    it "removes a tile from a rack" $ do
      let rack = [ Regular A, Regular B, Regular A, Blanco ]

      remove rack (Regular A) `shouldBe` [ Regular B, Regular A, Blanco ]
      remove rack (Regular B) `shouldBe` [ Regular A, Regular A, Blanco ]
      remove rack Blanco `shouldBe` [ Regular A, Regular B, Regular A ]
