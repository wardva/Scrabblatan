import Data.Foldable (traverse_)
import Data.List (tails)

main :: IO ()
main = do
  file <- getContents
  let dictionary = lines file
  let suffixTable = dictionary >>= suffixWordPairs
  traverse_ printSuffixWordPair suffixTable

suffixWordPairs :: String -> [(String, String)]
suffixWordPairs w = suffixes w `zip` repeat w

suffixes :: String -> [String]
suffixes = init . tails

printSuffixWordPair :: (String, String) -> IO ()
printSuffixWordPair (s, w) = let spaceLength = 40 - length s
                              in putStrLn $ s ++ replicate spaceLength ' ' ++ w
