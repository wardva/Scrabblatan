{-# LANGUAGE RecordWildCards #-}

import           Data.Foldable                (traverse_)
import           System.IO                    (getContents)
import           Text.ParserCombinators.ReadP

import qualified Scrabblatan.Dictionary       as Dict
import           Scrabblatan.Scrabble
import           Scrabblatan.Solver
import qualified Scrabblatan.SuffixMap      as Suff

data ScrabblatanOptions = ScrabblatanOptions
                        { hello      :: String
                        , quiet      :: Bool
                        , enthusiasm :: Int
                        }
  deriving (Show)

data ScrabblatanInput = ScrabblatanInput
                      { board :: Board
                      , rack  :: Rack
                      }

scrabblatanInputReadP :: ReadP ScrabblatanInput
scrabblatanInputReadP =
  ScrabblatanInput
    <$> readPBoard
    <*> (skipSpaces *> rackReadP)

instance Read ScrabblatanInput
  where readsPrec _ = readP_to_S scrabblatanInputReadP

instance Show ScrabblatanInput
  where show ScrabblatanInput {..} = show board <> "\n" <> show rack

main :: IO ()
main = do
  scrabblatanInputStr <- getContents
  let input@ScrabblatanInput {..} = read scrabblatanInputStr

  putStrLn (show input ++ "\n")

  putStrLn "Reading suffixtable from file"
  suffixTable <- Suff.fromFile "dictionaries/mini-suffixes"

  putStrLn "Reading dictionary from file"
  dictionary  <- Dict.fromFile "dictionaries/dutch-scrabble"

  putStrLn "Looking for solutions\n"
  let moves = solve suffixTable dictionary board rack

  traverse_ print moves
