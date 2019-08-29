{-# LANGUAGE OverloadedStrings #-}

module Scrabblatan.SuffixMap
  ( SuffixMap
  , contains
  , fromFile
  , findContaining
  ) where

import qualified Data.HashMap.Lazy         as Map
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as TextIO

import           Scrabblatan.Scrabble.Core

type SuffixMap = Map.HashMap ScrabbleWord [ScrabbleWord]

fromFile :: FilePath -> IO SuffixMap
fromFile filePath = do
  file <- TextIO.readFile filePath
  let lines' = Text.lines file
  let pairs  = (\(k,v) -> (read $ Text.unpack k, read . Text.unpack <$> v)) . fmap ((:[]) . Text.stripStart) . Text.breakOn "\t" <$> lines'
  let table  = Map.fromListWith (++) pairs :: SuffixMap

  return table

contains :: ScrabbleWord -> SuffixMap -> Bool
contains = Map.member

findContaining :: ScrabbleWord -> SuffixMap -> Maybe [ScrabbleWord]
findContaining = Map.lookup
