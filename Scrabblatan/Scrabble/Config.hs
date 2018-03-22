module Scrabblatan.Scrabble.Config
  ( boardFromEnv
  , boardFromEnv'
  , defaultBoard
  ) where

import           Data.Maybe           (fromMaybe)
import           Data.Vector          ((//))
import qualified Data.Vector          as Vector
import           System.Environment   (getEnvironment)

import           Scrabblatan.Scrabble
import           Scrabblatan.Util     (maybeRead)

type Environment = [(String, String)]

defaultBoardSize :: BoardSize
defaultBoardSize = 15

doubleCharPositionsKey, tripleCharPositionsKey, doubleWordPositionsKey, tripleWordPositionsKey, boardSizeKey :: String
doubleCharPositionsKey = "SCRABBLATAN_DOUBLE_CHAR_POSITIONS"
tripleCharPositionsKey = "SCRABBLATAN_TRIPLE_CHAR_POSITIONS"
doubleWordPositionsKey = "SCRABBLATAN_DOUBLE_WORD_POSITIONS"
tripleWordPositionsKey = "SCRABBLATAN_TRIPLE_WORD_POSITIONS"
boardSizeKey           = "SCRABBLATAN_BOARD_SIZE"

defaultDoubleCharPositions, defaultTripleCharPositions, defaultDoubleWordPositions, defaultTripleWordPositions :: [ Position ]
defaultDoubleCharPositions = [(0, 3), (2, 6), (3, 7), (6, 6)]
defaultTripleCharPositions = [(1, 5), (5, 5)]
defaultDoubleWordPositions = [(1, 1), (2, 2), (3, 3), (4, 4), (7, 7)]
defaultTripleWordPositions = [(0, 0), (0, 7)]

boardFromEnv' :: Environment -> Board
boardFromEnv' env =
  let boardSize           = readVar defaultBoardSize boardSizeKey env
      mirror'             = mirror boardSize
      doubleCharPositions = mirror' $ readVar defaultDoubleCharPositions doubleCharPositionsKey env
      tripleCharPositions = mirror' $ readVar defaultTripleCharPositions tripleCharPositionsKey env
      doubleWordPositions = mirror' $ readVar defaultDoubleWordPositions doubleWordPositionsKey env
      tripleWordPositions = mirror' $ readVar defaultTripleWordPositions tripleWordPositionsKey env
      cells               = Vector.generate (boardSize * boardSize) $ const emptyCell
      bonusses = [ (doubleCharPositions, CharacterBonus Double)
                 , (tripleCharPositions, CharacterBonus Triple)
                 , (doubleWordPositions, WordBonus Double)
                 , (tripleWordPositions, WordBonus Triple)
                 ] >>= uncurry (bonusCellUpdates boardSize)
   in Board { size = boardSize
            , getBoard = cells // bonusses
            }

boardFromEnv :: IO Board
boardFromEnv = boardFromEnv' <$> getEnvironment

defaultBoard :: Board
defaultBoard = boardFromEnv' mempty

readVar :: Read a => a -> String -> Environment -> a
readVar def key env = let val = lookup key env
                          err = "Invalid Scrabblatan configuration parameter: " ++ key
                       in maybe def (fromMaybe (error err) . maybeRead) val

mirror :: BoardSize -> [ Position ] -> [ Position ]
mirror n positions = positions >>= diag >>= vert >>= hor
  where h = (n `div` 2) + 1
        diag p@(r, c) | r == c = return p
                      | otherwise = [p, (c, r)]
        vert p@(r, c) | c == h    = return p
                      | otherwise = [p, (r, n - c - 1)]
        hor  p@(r, c) | r == h    = return p
                      | otherwise = [p, (n - r - 1, c)]
