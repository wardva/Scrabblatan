{-# LANGUAGE OverloadedStrings #-}

module Scrabblatan.SuffixTable
  ( SuffixTable
  , fromFile
  , findContaining
  , getAndSplit
  ) where
import           Data.Text    (isPrefixOf)
import qualified Data.Text    as Text
import qualified Data.Text.IO as TextIO
import           Data.Vector  (dropWhile, takeWhile, (!), (++))
import qualified Data.Vector  as Vector
import           Prelude      hiding (dropWhile, takeWhile, (++))

type SuffixEntry = (Text.Text, Text.Text)
type SuffixTable = Vector.Vector SuffixEntry

fromFile :: FilePath -> IO SuffixTable
fromFile filePath = do
  file <- TextIO.readFile filePath
  let entries = Text.lines file
  let pairs   = (fmap Text.stripStart . Text.breakOn "\t") <$> entries
  let table   = Vector.fromList pairs
  return table

findContaining :: Text.Text -> SuffixTable -> [Text.Text]
findContaining needle table
  | Vector.null table = []
  | otherwise = let idx = Vector.length table `div` 2
                    (left, entry, right) = getAndSplit table idx
                    suffix = fst entry
                    found = needle `isPrefixOf` suffix
                 in if found
                      then let leftWords = fst <$> left
                               rightWords = fst <$> right
                               rightResult = takeWhile (needle `isPrefixOf`) rightWords
                               leftResult = dropWhile (not . (needle `isPrefixOf`)) leftWords
                            in snd entry : Vector.toList (leftResult ++ rightResult)
                      else if needle < suffix
                              then findContaining needle left
                              else findContaining needle right


getAndSplit :: Vector.Vector a -> Int -> (Vector.Vector a, a, Vector.Vector a)
getAndSplit v i = let (left, right) = Vector.tail <$> Vector.splitAt i v
                      item = v ! i
                   in (left, item, right)
