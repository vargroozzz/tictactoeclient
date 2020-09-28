module UI
  ( mainUI
  )
where


import           Game
import           SizeSelect
import           SideSelect
import           NewGameSelect
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
import           Lens.Micro                     ( (&)
                                                , (%~)
                                                , ix
                                                )
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
    -- Move
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
  dSize <- defaultMain sizeSelectApp sizeSelectInitState
  dSide <- defaultMain sideSelectApp sideSelectInitState
  let size = parseSize . dialogSelection $ dSize
  side <- fromMaybe (return undefined) (dialogSelection dSide)
  let game' = case side of
        X -> return (mkGame size side)
        O -> updateField (mkGame size side)
  game     <- game'
  endGame  <- defaultMain app game
  nextStep <- defaultMain newGameSelectApp
                          (newGameSelectInitState . winner $ endGame)
  case dialogSelection nextStep of
    (Just NewGame) -> mainUI
    (Just Quit   ) -> putStrLn "Thx for game"
    _              -> putStrLn "Something went wrong, but thx for game"


 where
  head' [] = ' '
  head' x  = head x
  parseSize :: Maybe String -> Int
  parseSize (Just "3x3"  ) = 3
  parseSize (Just "5x5"  ) = 5
  parseSize (Just "7x7"  ) = 7
  parseSize (Just sizeStr) = read [head sizeStr]
  parseSize Nothing        = 0

winnerWidget :: Cell -> Widget ()
winnerWidget winner =
  withBorderStyle unicode
    .  border
    .  center
    .  str
    $  "The winner is: "
    ++ show winner
