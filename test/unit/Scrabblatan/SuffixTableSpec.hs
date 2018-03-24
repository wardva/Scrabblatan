{-# LANGUAGE OverloadedStrings #-}

module Scrabblatan.SuffixTableSpec where

import qualified Data.Vector                 as Vector
import           Test.Hspec

import           Scrabblatan.SuffixTable

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let dict = [ "mahler"
             , "bruckner"
             , "brahms"
             ]
  let table = generate dict

  describe "generate" $
    it "generates a valid suffix table" $ do
      let result = [ ("ahler", "mahler")
                   , ("ahms", "brahms")
                   , ("brahms", "brahms")
                   , ("bruckner", "bruckner")
                   , ("ckner", "bruckner")
                   , ("er", "bruckner")
                   , ("er", "mahler")
                   , ("hler", "mahler")
                   , ("hms", "brahms")
                   , ("kner", "bruckner")
                   , ("ler", "mahler")
                   , ("mahler", "mahler")
                   , ("ms", "brahms")
                   , ("ner", "bruckner")
                   , ("r", "bruckner")
                   , ("r", "mahler")
                   , ("rahms", "brahms")
                   , ("ruckner", "bruckner")
                   , ("s", "brahms")
                   , ("uckner", "bruckner")
                   ]

      Vector.toList table `shouldBe` result

  describe "findContaining" $ do
    it "returns multiple words" $ do
      let part = "ah"
      let result = [ "brahms"
                   , "mahler"
                   ]

      findContaining part table `shouldBe` result

    it "returns a single word" $ do
      let part = "kner"
      let result = [ "bruckner" ]

      findContaining part table `shouldBe` result

    it "returns repeated results when found multiple times" $ do
      let part = "r"
      let result = [ "brahms", "bruckner", "mahler" ]

      findContaining part table `shouldBe` result


    it "returns an empty array when nothing found" $ do
      let part = "eeth"
      let result = [ ]

      findContaining part table `shouldBe` result

  describe "contains" $ do
    it "returns true when one or more words were found" $ do
      let part = "r"
      contains part table `shouldBe` True

    it "returns false when no words were found" $ do
      let part = "zar"
      contains part table `shouldBe` False

