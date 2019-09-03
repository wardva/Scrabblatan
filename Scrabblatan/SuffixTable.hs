{-# LANGUAGE OverloadedStrings #-}

module Scrabblatan.SuffixTable
  ( SuffixTable
  , contains
  , fromFile
  , findContaining
  , generate
  , getAndSplit
  , sort
  , suffixEntries
  ) where
import           Data.Text                    (isPrefixOf)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TextIO
import           Data.Vector                  ((!))
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Merge as V.MergeSort

type SuffixEntry = (Text.Text, Text.Text)
type SuffixTable = V.Vector SuffixEntry

-- File should be sorted
fromFile :: FilePath -> IO SuffixTable
fromFile filePath = do
  file <- TextIO.readFile filePath
  let lines' = Text.lines file
  let pairs  = fmap Text.stripStart . Text.breakOn "\t" <$> lines'
  let table  = V.fromList pairs
  return table

generate :: [Text.Text] -> SuffixTable
generate ws = sort . V.fromList $ ws >>= suffixEntries

getAndSplit :: V.Vector a -> Int -> (V.Vector a, a, V.Vector a)
getAndSplit v i = let (left, right) = V.tail <$> V.splitAt i v
                      item = v ! i
                   in (left, item, right)

contains :: Text.Text -> SuffixTable -> Bool
contains needle table = not . null $ findContaining needle table

findContaining :: Text.Text -> SuffixTable -> [Text.Text]
findContaining needle table
  | V.null table = []
  | otherwise = let (left, entry, right) = getAndSplit table (V.length table `div` 2)
                    suffix = fst entry
                    found = needle `isPrefixOf` suffix
                 in if found
                      then let rightResult = V.takeWhile ((needle `isPrefixOf`) . fst) right
                               leftResult = V.dropWhile (not . (needle `isPrefixOf`) . fst) left
                            in V.toList . V.uniq . sort . fmap snd $ V.cons entry $ leftResult V.++ rightResult
                      else findContaining needle (if needle < suffix then left else right)

suffixes :: Text.Text -> [Text.Text]
suffixes = init . Text.tails

suffixEntries :: Text.Text -> [SuffixEntry]
suffixEntries w = suffixes w `zip` repeat w

sort :: Ord a => V.Vector a -> V.Vector a
sort = V.modify V.MergeSort.sort
