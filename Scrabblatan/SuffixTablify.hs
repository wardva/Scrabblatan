{-# LANGUAGE OverloadedStrings #-}

import           Data.Foldable           (traverse_)
import           Data.Monoid             ((<>))
import qualified Data.Text               as Text
import qualified Data.Text.IO            as TextIO

import           Scrabblatan.SuffixTable

main :: IO ()
main = do
  file <- TextIO.getContents
  let dictionary = Text.lines file
  let suffixTable = dictionary >>= suffixEntries
  traverse_ printSuffixWordPair suffixTable

printSuffixWordPair :: (Text.Text, Text.Text) -> IO ()
printSuffixWordPair (s, w) = TextIO.putStrLn (s <> "\t" <> w)
