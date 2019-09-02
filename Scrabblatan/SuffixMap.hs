{-# LANGUAGE OverloadedStrings #-}

module Scrabblatan.SuffixMap
  ( SuffixMap
  , contains
  , fromFile
  ) where

import           Scrabblatan.Scrabble

import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TextIO
import qualified Data.HashMap.Strict          as Map

type SuffixMap = Map.HashMap ScrabbleWord [ScrabbleWord]

-- File should be sorted
fromFile :: FilePath -> IO SuffixMap
fromFile filePath = do
  file <- TextIO.readFile filePath
  let lines' = Text.lines file
  let pairs = (\(k,v) -> (read $ Text.unpack k, [read . Text.unpack $ Text.stripStart v])) . Text.breakOn "\t" <$> lines'

  return $ Map.fromListWith (++) pairs

contains :: ScrabbleWord -> SuffixMap -> Bool
contains = Map.member
