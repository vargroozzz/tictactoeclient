module UI
  ( mainUI
  )
where

import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           Game
import           SizeSelect
import           Brick
import           Brick.Widgets.Border           ( border
                                                , borderWithLabel
                                                , hBorderWithLabel
                                                , vBorder
                                                )
import           Brick.Widgets.Border.Style     ( unicode
                                                , unicodeBold
                                                )
import           Brick.Widgets.Center           ( center )
import           Data.List                      ( intersperse )
import           Data.List.Split                ( chunksOf )
import qualified Graphics.Vty                  as V
import           Lens.Micro
import           Brick.Widgets.Dialog           ( dialogSelection )
import           Data.Maybe                     ( fromMaybe )

styleCursor, styleCellGiven, styleCellInput, styleCellNote :: AttrName
styleSolved, styleUnsolved :: AttrName
styleCursor = attrName "styleCursor"
styleCellGiven = attrName "styleCellGiven"
styleCellInput = attrName "styleCellInput"
styleCellNote = attrName "styleCellNote"
styleSolved = attrName "styleSolved"
styleUnsolved = attrName "styleUnsolved"

attributes :: AttrMap
attributes = attrMap
  V.defAttr
  [ (styleCursor   , bg V.brightBlack)
  , (styleCellGiven, V.defAttr)
  , (styleCellInput, fg V.blue)
  , (styleCellNote , fg V.yellow)
  , (styleSolved   , fg V.green)
  , (styleUnsolved , fg V.red)
  ]

handleEvent :: Game -> BrickEvent () e -> EventM () (Next Game)
handleEvent (Game c s g (Just w)) _ = halt (Game c s g (Just w))
handleEvent game (VtyEvent (V.EvKey key [V.MCtrl])) = case key of
    -- Quit
  V.KChar 'c' -> halt game

  -- Reset
  -- V.KChar 'r' -> continue . resetGame $ game
  -- Other
  _           -> continue game



handleEvent game (VtyEvent (V.EvKey V.KEnter [])) =
  let (x, y) = cursor game
  in  case grid game !! y !! x of
        Empty ->
          suspendAndResume
            . updateField
            . answerCell (head . show . side $ game)
            $ game
        _ -> continue game

handleEvent game (VtyEvent (V.EvKey key [])) = continue $ case key of
    -- Move by cell
  V.KUp       -> moveCursor North game
  V.KDown     -> moveCursor South game
  V.KLeft     -> moveCursor West game
  V.KRight    -> moveCursor East game
  V.KChar 'k' -> moveCursor North game
  V.KChar 'j' -> moveCursor South game
  V.KChar 'h' -> moveCursor West game
  V.KChar 'l' -> moveCursor East game
  V.KChar 'w' -> moveCursor North game
  V.KChar 's' -> moveCursor South game
  V.KChar 'a' -> moveCursor West game
  V.KChar 'd' -> moveCursor East game
  -- Other
  _           -> game
handleEvent game _ = continue game

highlightCursor :: Game -> [[[Widget ()]]] -> [[[Widget ()]]]
highlightCursor game widgets =
  widgets & ix bigCol . ix smallRow . ix smallCol %~ withDefAttr styleCursor
 where
  (x, y)   = cursor game
  bigCol   = x `div` getSize game
  smallRow = y `mod` getSize game
  smallCol = x `mod` getSize game

drawCell :: Cell -> Widget ()
drawCell cell = center $ case cell of
  Input x -> withAttr styleCellInput . str $ show x
  Empty   -> withAttr styleCellInput . str $ " "


drawGrid :: Game -> Widget ()
drawGrid game =
  getRegion game
    & chunksOf (getSize game)
    & fmap (fmap (fmap drawCell))
    & highlightCursor game
    & fmap (fmap (intersperse (withBorderStyle unicode vBorder)))
    & fmap (fmap hBox)
    & fmap
        (intersperse
          (withBorderStyle unicode (hBorderWithLabel (str "─────────")))
        )
    & fmap vBox
    & intersperse (withBorderStyle unicodeBold vBorder)
    & hBox
    -- & intersperse
    --     (withBorderStyle unicodeBold
    --                      (hBorderWithLabel (str "╋━━━━━━━━━━━━━━━━━━━━━━━╋"))
    --     )
    -- & vBox
    & border
    & withBorderStyle unicodeBold
    & setAvailableSize (50, 26)
    & padRight (Pad 1)

drawHelp :: Widget ()
drawHelp =
  ["move:    ←↓↑→ / wasd / hjkl", "answer:  Enter", "quit:    ctrl + c"]
    & unlines
    & str
    & padLeftRight 1
    & borderWithLabel (str " Help ")
    & withBorderStyle unicodeBold
    & setAvailableSize (31, 12)

drawDebug :: Game -> Widget ()
drawDebug game =
  [ "cursor:    (" <> show x <> ", " <> show y <> ")"
    , "winner:    " <> maybe "" show (gameSolved game)
    ]
    & unlines
    & str
    & padRight Max
    & padLeftRight 1
    & borderWithLabel (str " Debug ")
    & withBorderStyle unicodeBold
    & hLimit 31
  where (x, y) = cursor game



drawUI :: Game -> Widget ()
drawUI game = drawGrid game <+> (drawHelp <=> drawDebug game)

app :: App Game e ()
app = App { appDraw         = \x -> [drawUI x]
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = pure
          , appAttrMap      = const attributes
          }

mainUI :: IO ()
mainUI = do
  d <- defaultMain sizeSelectApp sizeSelectInitState
  let size = parseSize . dialogSelection $ d
  putStr $ unlines
    [ "SUDOKU"
    , "  1) Start game for X"
    , "  2) Start game for O"
    , "  3) Start game for random side"
    , "  *) Quit"
    ]
  response <- prompt "> "
  case head' response of
    '1' -> do
      victor <- defaultMain app (mkGame . table $ size)
      simpleMain . winnerWidget . cellFromMaybeSide . winner $ victor
      mainUI
    '2' -> do
      victor <- defaultMain app ((mkGame . table $ size) { side = O })
      simpleMain . winnerWidget . cellFromMaybeSide . winner $ victor
      mainUI
    '3' -> do
      rSide  <- randSide
      victor <- defaultMain app ((mkGame . table $ size) { side = rSide })
      simpleMain . winnerWidget . cellFromMaybeSide . winner $ victor
      mainUI
    _ -> putStrLn "Quitting..."
 where
  head' [] = ' '
  head' x  = head x
  parseSize :: Maybe String -> Int
  parseSize (Just "3x3"  ) = 3
  parseSize (Just "5x5"  ) = 5
  parseSize (Just "7x7"  ) = 7
  parseSize (Just sizeStr) = read [head sizeStr]
  parseSize Nothing        = 0


table :: Int -> [String]
table size = replicate size . replicate size $ ' '

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

winnerWidget :: Cell -> Widget ()
winnerWidget winner =
  withBorderStyle unicode
    .  border
    .  center
    .  str
    $  "The winner is: "
    ++ show winner
