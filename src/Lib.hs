{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( runUI
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
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import qualified Brick.Widgets.Center          as C
import           Brick.Widgets.Core             ( joinBorders
                                                , padAll
                                                , str
                                                )
import qualified Brick.Widgets.Dialog          as D
import           Data.Aeson                     ( Value )
import qualified Data.ByteString.Char8         as S8
import qualified Data.ByteString.Lazy.Char8    as L8
import qualified Data.ByteString.Lazy.Internal as LI
import qualified Data.Yaml                     as Yaml
import qualified Graphics.Vty                  as V
import           Network.HTTP.Simple

-- data Choice = ThreeXThree | FiveXFive | SevenXSeven
--   deriving (Show)
-- type Choice = String

drawUI :: D.Dialog String -> [Widget ()]
drawUI d = [ui]
 where
  ui = D.renderDialog d $ C.hCenter $ padAll 1 $ str "Choose the table size."

appEvent
  :: D.Dialog String
  -> BrickEvent () e
  -> T.EventM () (T.Next (D.Dialog String))
appEvent d (VtyEvent ev) = case ev of
  V.EvKey V.KEsc   [] -> M.halt d
  V.EvKey V.KEnter [] -> M.halt d
  _                   -> M.continue =<< D.handleDialogEvent ev d
appEvent d _ = M.continue d

initialState :: D.Dialog String
initialState = D.dialog (Just "Title") (Just (0, choices)) 50
  where choices = [("3x3", "3x3"), ("5x5", "5x5"), ("7x7", "7x7")]

theMap :: A.AttrMap
theMap = A.attrMap
  V.defAttr
  [ (D.dialogAttr        , V.white `on` V.blue)
  , (D.buttonAttr        , V.black `on` V.white)
  , (D.buttonSelectedAttr, bg V.yellow)
  , (cellAttr            , V.white `on` V.black)
  , (cellSelectedAttr    , V.black `on` V.yellow)
  ]

cellAttr, cellSelectedAttr :: AttrName
cellAttr = attrName "cellAttr"
cellSelectedAttr = attrName "cellAttr"

theApp :: M.App (D.Dialog String) e ()
theApp = M.App { M.appDraw         = drawUI
               , M.appChooseCursor = M.showFirstCursor
               , M.appHandleEvent  = appEvent
               , M.appStartEvent   = return
               , M.appAttrMap      = const theMap
               }

runUI :: IO ()
runUI = do
  d        <- M.defaultMain theApp initialState
  request' <- parseRequest "GET http://127.0.0.1:8080/table"
  let request =
        setRequestQueryString [parseTable (D.dialogSelection d)] request'
  response <- httpJSON request
  simpleMain . drawField $ (getResponseBody response)
 where
  parseTable (Just "3x3") = ("size", Just "3x3")
  parseTable (Just "5x5") = ("size", Just "5x5")
  parseTable (Just "7x7") = ("size", Just "7x7")

ui :: Widget ()
ui = drawField [[" ", " ", " "], [" ", " ", " "], [" ", " ", " "]]

-- center (str "") <+> vBorder <+> center (str "") <+> vBorder <+> center (str "")

debugUI :: IO ()
debugUI = simpleMain ui

-- drawField :: [[String]] -> Widget ()
-- drawField = foldr (\(s1 : s2 : [s3]) acc -> acc <=> hBorder <+> (padAll 1 (center (str s1)) <+> padAll 1 (center (str s2)) <+> vBorder <+> padAll 1 (center (str s3)))) hBorder

drawField :: [[String]] -> Widget ()
drawField =
  joinBorders
    . withBorderStyle unicodeBold
    . border
    . vLimit 30
    . hLimit 32
    . vBox
    . mapHelper
 where
  mapHelper [(s1 : s2 : [s3])] =
    [ (setAvailableSize (10, 10) . padAll 1 . center . str $ s1)
        <+> vBorder
        <+> (setAvailableSize (10, 10) . padAll 1 . center . str $ s2)
        <+> vBorder
        <+> (setAvailableSize (10, 10) . padAll 1 . center . str $ s3)
    ]
  mapHelper ((s1 : s2 : [s3]) : rows) =
    (setAvailableSize (10, 10) . padAll 1 . center . str $ s1)
      <+> vBorder
      <+> (setAvailableSize (10, 10) . padAll 1 . center . str $ s2)
      <+> vBorder
      <+> (setAvailableSize (10, 10) . padAll 1 . center . str $ s3)
      <=> hBorder
      :   mapHelper rows

