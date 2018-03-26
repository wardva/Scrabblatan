module Scrabblatan.Scrabble.Config
  ( boardFromEnv
  , boardFromEnv'
  , tileValuesFromEnv'
  , tileValuesFromEnv
  ) where

import           Data.Maybe           (fromMaybe)
import           Data.Vector          ((//))
import qualified Data.Vector          as Vector
import           System.Environment   (getEnvironment)

import           Scrabblatan.Scrabble
import           Scrabblatan.Util     (maybeRead)

type Environment = [(String, String)]

defaultBoardSize :: Int
defaultBoardSize = 15

doubleCharPositionsKey, tripleCharPositionsKey, doubleWordPositionsKey, tripleWordPositionsKey, boardSizeKey, tileValuesKey :: String
doubleCharPositionsKey = "SCRABBLATAN_DOUBLE_CHAR_POSITIONS"
tripleCharPositionsKey = "SCRABBLATAN_TRIPLE_CHAR_POSITIONS"
doubleWordPositionsKey = "SCRABBLATAN_DOUBLE_WORD_POSITIONS"
tripleWordPositionsKey = "SCRABBLATAN_TRIPLE_WORD_POSITIONS"
boardSizeKey           = "SCRABBLATAN_BOARD_SIZE"
tileValuesKey          = "SCRABBLATAN_TILE_VALUES"

defaultDoubleCharPositions, defaultTripleCharPositions, defaultDoubleWordPositions, defaultTripleWordPositions :: [ Position ]
defaultDoubleCharPositions = [(0, 3), (2, 6), (3, 7), (6, 6)]
defaultTripleCharPositions = [(1, 5), (5, 5)]
defaultDoubleWordPositions = [(1, 1), (2, 2), (3, 3), (4, 4), (7, 7)]
defaultTripleWordPositions = [(0, 0), (0, 7)]

defaultTileValues :: [(Tile, Int)]
defaultTileValues = [ (Regular A, 1), (Regular B, 3), (Regular C, 5), (Regular D, 2), (Regular E, 1),  (Regular F, 4)
                    , (Regular G, 3), (Regular H, 4), (Regular I, 1), (Regular J, 4), (Regular K, 3),  (Regular L, 3)
                    , (Regular M, 3), (Regular N, 1), (Regular O, 1), (Regular P, 3), (Regular Q, 10), (Regular S, 2)
                    , (Regular T, 2), (Regular U, 4), (Regular V, 4), (Regular W, 5), (Regular X, 8),  (Regular Y, 8)
                    , (Regular Z, 4), (Blanco, 0)
                    ]

tileValuesFromEnv' :: Environment -> TileValues
tileValuesFromEnv' = readVar defaultTileValues tileValuesKey

tileValuesFromEnv :: IO TileValues
tileValuesFromEnv = tileValuesFromEnv' <$> getEnvironment

boardFromEnv' :: Environment -> Board
boardFromEnv' env =
  let boardSize'          = readVar defaultBoardSize boardSizeKey env
      mirror'             = mirror boardSize'
      doubleCharPositions = mirror' $ readVar defaultDoubleCharPositions doubleCharPositionsKey env
      tripleCharPositions = mirror' $ readVar defaultTripleCharPositions tripleCharPositionsKey env
      doubleWordPositions = mirror' $ readVar defaultDoubleWordPositions doubleWordPositionsKey env
      tripleWordPositions = mirror' $ readVar defaultTripleWordPositions tripleWordPositionsKey env
      cells               = Vector.generate (boardSize' * boardSize') $ const emptyCell
      bonusses = [ (doubleCharPositions, CharacterBonus Double)
                 , (tripleCharPositions, CharacterBonus Triple)
                 , (doubleWordPositions, WordBonus Double)
                 , (tripleWordPositions, WordBonus Triple)
                 ] >>= uncurry (bonusCellUpdates boardSize')
   in Board { boardSize = boardSize'
            , getBoard = cells // bonusses
            }

boardFromEnv :: IO Board
boardFromEnv = boardFromEnv' <$> getEnvironment

readVar :: Read a => a -> String -> Environment -> a
readVar def key env = let val = lookup key env
                          err = "Invalid Scrabblatan configuration parameter: " ++ key
                       in maybe def (fromMaybe (error err) . maybeRead) val

mirror :: Int -> [ Position ] -> [ Position ]
mirror n positions = positions >>= diag >>= vert >>= hor
  where h = (n `div` 2) + 1
        diag p@(r, c) | r == c = return p
                      | otherwise = [p, (c, r)]
        vert p@(r, c) | c == h    = return p
                      | otherwise = [p, (r, n - c - 1)]
        hor  p@(r, c) | r == h    = return p
                      | otherwise = [p, (n - r - 1, c)]
