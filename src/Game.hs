{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Game
  ( Cell(..)
  , Side(..)
  , Row
  , Grid
  , Game(..)
  , Direction(..)
  , mkGame
  , moveCursor
  , answerCell
  , updateField
  , gameSolved
  , getRegion
  , getSize
  , randSide
  )
where
import           Data.Maybe                     ( fromMaybe )
import           System.Random                  ( Random(randomIO) )
import           Data.Function                  ( (&) )
import           Data.Aeson
import           Data.List                      ( transpose
                                                , nub
                                                )
import           Lens.Micro                     ( ix
                                                , (%~)
                                                )
import           Network.HTTP.Simple
import           GHC.Generics                   ( Generic )
import           GHC.Read
import qualified Text.Read.Lex                 as L
import           Text.Read                      ( readMaybe
                                                , pfail
                                                )
import           Network.HTTP.Client.Conduit    ( Request(requestBody) )

data TurnReq = TurnReq { sideTurn :: Side, gridReq :: [[String]] } deriving (Generic, Show)
data TurnRes = TurnRes { sideWon :: Maybe Side, gridRes :: [[String]] } deriving (Generic, Show)

instance ToJSON Side where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Side

instance ToJSON TurnReq where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TurnReq

instance ToJSON TurnRes where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TurnRes

data Side = X | O deriving (Generic, Eq, Read, Show)

data Cell = Input Side | Empty
  deriving (Eq)

instance Show Cell where
  show (Input X) = "X"
  show (Input O) = "O"
  show Empty     = " "

instance Read Cell where
  readPrec = parens
    (do
      L.Ident s <- lexP
      case s of
        "X" -> return (Input X)
        "O" -> return (Input O)
        " " -> return Empty
        _   -> pfail
    )

type Row = [Cell]

type Grid = [Row]

data Game = Game
  { cursor :: (Int, Int)
  , side :: Side
  , grid :: Grid
  , winner :: Maybe Side
  } deriving (Read, Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Read, Show)


mkGame :: Int -> Side -> Game
mkGame size s =
  Game { cursor = (0, 0), side = s, grid = table size, winner = Nothing }

moveCursor :: Direction -> Game -> Game
moveCursor direction game = (\c -> game { cursor = c }) $ case direction of
  North -> (x, wrap (y - 1))
  South -> (x, wrap (y + 1))
  East  -> (wrap (x + 1), y)
  West  -> (wrap (x - 1), y)
 where
  size   = getSize game
  (x, y) = cursor game
  wrap n | n >= size = n - size
         | n < 0     = n + size
         | otherwise = n

transformCell :: (Cell -> Cell) -> Game -> Game
transformCell f game = game { grid = grid game & ix y . ix x %~ fun }
 where
  (x, y) = cursor game
  fun    = case grid game !! y !! x of
    Empty -> f
    _     -> id

answerCell :: Char -> Game -> Game
answerCell ch = transformCell $ \case
  _ -> Input . read $ [ch]



gameSolved :: Game -> Maybe Side
gameSolved game | Just X `elem` solveds = Just X
                | Just O `elem` solveds = Just O
                | otherwise             = Nothing
 where
  rowsSolved         = solved <$> grid game
  columnsSolved      = solved <$> (transpose . grid $ game)
  mainDiagonalSolved = solved . getDiagonal . grid $ game
  sideDiagonalSolved = solved . getDiagonal $ (reverse <$> grid game)
  solved row | row == xs = Just X
             | row == os = Just O
             | otherwise = Nothing
  solved' row =
    ((length . filter (== Input X) $ row) >= 3)
      || ((length . filter (== Input O) $ row) >= 3)
  xs = replicate (getSize game) (Input X)
  os = replicate (getSize game) (Input O)
  getDiagonal xs = zipWith (!!) xs [0 ..]
  solveds =
    mainDiagonalSolved : sideDiagonalSolved : rowsSolved ++ columnsSolved


getColumns :: Game -> [[Cell]]
getColumns game =
  [ [ grid game !! row !! column | row <- [0 .. (size - 1)] ]
  | column <- [0 .. (size - 1)]
  ]
  where size = getSize game

getRegion :: Game -> [[Cell]]
getRegion game =
  [ [ grid game !! row !! col | col <- [0 .. (size - 1)] ]
  | row <- [0 .. (size - 1)]
  ]
  where size = getSize game

getSize :: Game -> Int
getSize = length . grid

randSide :: IO Side
randSide = do
  num <- randomIO :: IO Int
  return
    (case num `mod` 2 of
      0 -> X
      1 -> O
      _ -> undefined
    )

updateField :: Game -> IO Game
updateField game = do
  request' <- parseRequest "GET http://127.0.0.1:8080/game"
  let request = setRequestBodyJSON
        (TurnReq (side game) (fmap show <$> grid game))
        request'
  response <- httpJSON request :: IO (Response TurnRes)
  return game { grid = fmap readCell <$> (gridRes . getResponseBody $ response)
              , winner = sideWon . getResponseBody $ response
              }

readCell :: String -> Cell
readCell cell = fromMaybe Empty (readMaybe cell)

table :: Int -> [[Cell]]
table size = replicate size . replicate size $ Empty
