{-# LANGUAGE NoImplicitPrelude #-}

module UI (runUI) where

import Brick
import qualified Brick.AttrMap as A
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Pomodoro
import Relude
import UI.Style (attributes)
import UI.View (viewUI)

data AppEvent = Tick deriving (Eq, Show)

uiApp :: App Pomodoro AppEvent ()
uiApp =
  App
    { appDraw = viewUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const $ A.attrMap V.defAttr attributes
    }

runUI :: IO ()
runUI = do
  vty <- mkVty V.defaultConfig
  bchan <- newBChan 1
  ticker <- async $ tickerLoop bchan
  void $ customMain vty (mkVty V.defaultConfig) (Just bchan) uiApp initialState
  cancel ticker

tickerLoop :: BChan AppEvent -> IO ()
tickerLoop bchan = forever $ do
  threadDelay 1000000
  writeBChan bchan Tick

handleEvent :: BrickEvent () AppEvent -> EventM () Pomodoro ()
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = modify startOrPause
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = modify reset
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent _) = pure ()
handleEvent (AppEvent Tick) = modify tick
handleEvent (MouseDown {}) = pure ()
handleEvent (MouseUp {}) = pure ()
