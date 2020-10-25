{-# LANGUAGE OverloadedStrings #-}
module SideSelect
  ( sideSelectApp
  , sideSelectInitState
  )
where

import           Game                           ( Side(..)
                                                , randSide
                                                )
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

drawUI :: Dialog (IO Side) -> [Widget ()]
drawUI d = [ui]
  where ui = renderDialog d $ hCenter $ padAll 1 $ str "Choose your side."

sideSelectEvent
  :: Dialog (IO Side) -> BrickEvent () e -> EventM () (Next (Dialog (IO Side)))
sideSelectEvent d (VtyEvent ev) = case ev of
  EvKey KEsc   [] -> halt d
  EvKey KEnter [] -> halt d
  _               -> continue =<< handleDialogEvent ev d
sideSelectEvent d _ = continue d

sideSelectInitState :: Dialog (IO Side)
sideSelectInitState = dialog Nothing (Just (0, choices)) 50
  where choices = [("X", return X), ("O", return O), ("Random", randSide)]

sideSelectMap :: AttrMap
sideSelectMap = attrMap
  defAttr
  [ (dialogAttr        , white `on` black)
  , (buttonAttr        , white `on` black)
  , (buttonSelectedAttr, black `on` white)
  ]

sideSelectApp :: App (Dialog (IO Side)) e ()
sideSelectApp = App { appDraw         = drawUI
                    , appChooseCursor = neverShowCursor
                    , appHandleEvent  = sideSelectEvent
                    , appStartEvent   = return
                    , appAttrMap      = const sideSelectMap
                    }
