
module UI.Dialog
  ( dialogApp
  , dialogInitState
  )
where

import           Brick
import qualified Brick.AttrMap                 as A
import qualified Brick.Main                    as M
import           Brick.Types                    ( BrickEvent(..)
                                                , Widget
                                                )
import qualified Brick.Types                   as T
import           Brick.Util                     ( bg
                                                , on
                                                )
import qualified Brick.Widgets.Center          as C
import           Brick.Widgets.Core             ( joinBorders
                                                , padAll
                                                , str
                                                )
import qualified Brick.Widgets.Dialog          as D
import qualified Graphics.Vty                  as V

drawUI :: D.Dialog String -> [Widget ()]
drawUI d = [ui]
 where
  ui = D.renderDialog d $ C.hCenter $ padAll 1 $ str "Choose the table size."

dialogEvent
  :: D.Dialog String
  -> BrickEvent () e
  -> T.EventM () (T.Next (D.Dialog String))
dialogEvent d (VtyEvent ev) = case ev of
  V.EvKey V.KEsc   [] -> M.halt d
  V.EvKey V.KEnter [] -> M.halt d
  _                   -> M.continue =<< D.handleDialogEvent ev d
dialogEvent d _ = M.continue d

dialogInitState :: D.Dialog String
dialogInitState = D.dialog (Just "Title") (Just (0, choices)) 50
  where choices = [("3x3", "3x3"), ("5x5", "5x5"), ("7x7", "7x7")]

dialogMap :: A.AttrMap
dialogMap = A.attrMap
  V.defAttr
  [ (D.dialogAttr        , V.white `on` V.blue)
  , (D.buttonAttr        , V.black `on` V.white)
  , (D.buttonSelectedAttr, bg V.yellow)
  ]

dialogApp :: M.App (D.Dialog String) e ()
dialogApp = M.App { M.appDraw         = drawUI
                  , M.appChooseCursor = M.neverShowCursor
                  , M.appHandleEvent  = dialogEvent
                  , M.appStartEvent   = return
                  , M.appAttrMap      = const dialogMap
                  }




