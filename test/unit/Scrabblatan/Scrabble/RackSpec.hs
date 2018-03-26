{-# LANGUAGE OverloadedStrings #-}

module Scrabblatan.Scrabble.RackSpec where

import           Test.Hspec

import           Scrabblatan.Scrabble

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "available" $ do
    it "returns True if character is available" $ do
      let rack = [ Regular A, Regular B, Regular A
                 , Regular Q, Regular W, Regular E
                 ]

      available rack A `shouldBe` True
      available rack Q `shouldBe` True
      available rack E `shouldBe` True

    it "returns False if character is available" $ do
      let rack = [ Regular A, Regular B, Regular A
                 , Regular Q, Regular W, Regular E
                 ]

      available rack Z `shouldBe` False
      available rack N `shouldBe` False

    it "always returns False when rack is empty" $ do
      let rack = []

      available rack A `shouldBe` False
      available rack Q `shouldBe` False
      available rack E `shouldBe` False

    it "always returns True when rack contains Blanco" $ do
      let rack = [ Blanco, Regular Z]

      available rack A `shouldBe` True
      available rack Q `shouldBe` True
      available rack E `shouldBe` True

  describe "timesAvailable" $ do
    it "returns the number of times a character is available (no Blanco on rack)" $ do
      let rack = [ Regular A, Regular B, Regular A
                 , Regular Q, Regular W, Regular E
                 ]

      timesAvailable rack A `shouldBe` 2
      timesAvailable rack B `shouldBe` 1
      timesAvailable rack Q `shouldBe` 1
      timesAvailable rack V `shouldBe` 0
      timesAvailable rack Z `shouldBe` 0

    it "returns the number of times a character is available (with Blanco on rack)" $ do
      let rack = [ Regular A, Blanco, Regular A
                 , Regular Q, Regular W, Blanco
                 ]

      timesAvailable rack A `shouldBe` 4
      timesAvailable rack B `shouldBe` 2
      timesAvailable rack Q `shouldBe` 3
      timesAvailable rack V `shouldBe` 2
      timesAvailable rack Z `shouldBe` 2

