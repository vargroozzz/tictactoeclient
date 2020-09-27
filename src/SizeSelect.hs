{-# LANGUAGE OverloadedStrings #-}
module SizeSelect
  ( sizeSelectApp
  , sizeSelectInitState
  )
where

import           Brick                          ( attrMap
                                                , continue
                                                , halt
                                                , neverShowCursor
                                                , bg
                                                , on
                                                , padAll
                                                , str
                                                , AttrMap
                                                , App(..)
                                                , EventM
                                                , Widget
                                                , BrickEvent(VtyEvent)
                                                , Next
                                                )
import           Brick.Widgets.Center           ( hCenter )
import           Brick.Widgets.Dialog           ( buttonAttr
                                                , buttonSelectedAttr
                                                , dialog
                                                , dialogAttr
                                                , handleDialogEvent
                                                , renderDialog
                                                , Dialog
                                                )
import           Graphics.Vty                   ( defAttr
                                                , Event(EvKey)
                                                , black
                                                , blue
                                                , white
                                                , yellow
                                                , Key(KEnter, KEsc)
                                                )

drawUI :: Dialog String -> [Widget ()]
drawUI d = [ui]
  where ui = renderDialog d $ hCenter $ padAll 1 $ str "Choose the table size."

sizeSelectEvent
  :: Dialog String -> BrickEvent () e -> EventM () (Next (Dialog String))
sizeSelectEvent d (VtyEvent ev) = case ev of
  EvKey KEsc   [] -> halt d
  EvKey KEnter [] -> halt d
  _               -> continue =<< handleDialogEvent ev d
sizeSelectEvent d _ = continue d

sizeSelectInitState :: Dialog String
sizeSelectInitState = dialog Nothing (Just (0, choices)) 50
  where choices = [("3x3", "3x3"), ("5x5", "5x5"), ("7x7", "7x7")]

sizeSelectMap :: AttrMap
sizeSelectMap = attrMap
  defAttr
  [ (dialogAttr        , white `on` blue)
  , (buttonAttr        , black `on` white)
  , (buttonSelectedAttr, bg yellow)
  ]

sizeSelectApp :: App (Dialog String) e ()
sizeSelectApp = App { appDraw         = drawUI
                    , appChooseCursor = neverShowCursor
                    , appHandleEvent  = sizeSelectEvent
                    , appStartEvent   = return
                    , appAttrMap      = const sizeSelectMap
                    }
