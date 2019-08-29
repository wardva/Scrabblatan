module Scrabblatan.Dictionary
  ( Dictionary
  , contains
  , fromFile
  ) where

import qualified Data.HashSet              as Set
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as TextIO

import           Scrabblatan.Scrabble.Core

type Dictionary = Set.HashSet ScrabbleWord

fromFile :: FilePath -> IO Dictionary
fromFile filePath = do
  file <- TextIO.readFile filePath
  let lines' = Text.lines file
  let table  = Set.fromList (read . Text.unpack <$> lines')
  return table

contains :: ScrabbleWord -> Dictionary -> Bool
contains = Set.member
