{-# LANGUAGE OverloadedStrings #-}
module NewGameSelect
  ( newGameSelectApp
  , newGameSelectInitState
  , Chosen(..)
  )
where

import           Game                           ( Side(..) )

import           Brick                          ( App(..)
                                                , attrMap
                                                , continue
                                                , halt
                                                , neverShowCursor
                                                , on
                                                , padAll
                                                , str
                                                , AttrMap
                                                , EventM
                                                , Widget
                                                , BrickEvent(VtyEvent)
                                                , Next
                                                )
import           Brick.Widgets.Center           ( hCenter )
import           Brick.Widgets.Dialog           ( Dialog
                                                , buttonAttr
                                                , buttonSelectedAttr
                                                , dialog
                                                , dialogAttr
                                                , handleDialogEvent
                                                , renderDialog
                                                )
import           Graphics.Vty                   ( defAttr
                                                , Event(EvKey)
                                                , black
                                                , white
                                                , Key(KEnter, KEsc)
                                                )


data Chosen = NewGame | Quit

drawUI :: Dialog Chosen -> [Widget ()]
drawUI d = [ui]
 where
  ui =
    renderDialog d $ hCenter $ padAll 1 $ str "Do you want to start new game?"

newGameSelectEvent
  :: Dialog Chosen -> BrickEvent () e -> EventM () (Next (Dialog Chosen))
newGameSelectEvent d (VtyEvent ev) = case ev of
  EvKey KEsc   [] -> halt d
  EvKey KEnter [] -> halt d
  _               -> continue =<< handleDialogEvent ev d
newGameSelectEvent d _ = continue d

newGameSelectInitState :: Maybe Side -> Dialog Chosen
newGameSelectInitState winner = dialog
  ((\s -> "The winner is: " ++ show s) <$> winner)
  (Just (0, choices))
  50
  where choices = [("Start new game", NewGame), ("Quit", Quit)]



newGameSelectMap :: AttrMap
newGameSelectMap = attrMap
  defAttr
  [ (dialogAttr        , white `on` black)
  , (buttonAttr        , white `on` black)
  , (buttonSelectedAttr, black `on` white)
  ]

newGameSelectApp :: App (Dialog Chosen) e ()
newGameSelectApp = App { appDraw         = drawUI
                       , appChooseCursor = neverShowCursor
                       , appHandleEvent  = newGameSelectEvent
                       , appStartEvent   = return
                       , appAttrMap      = const newGameSelectMap
                       }
