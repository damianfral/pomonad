{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI (runUI) where

import Brick
import qualified Brick.AttrMap as A
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Pomodoro
import Relude
import System.Process.Typed
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
  playNotificationSound
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
handleEvent (AppEvent Tick) = do
  modify tick
  Pomodoro {..} <- get
  liftIO $ case pomodoroPhase of
    Focus 0 -> playNotificationSound
    Break 0 -> playNotificationSound
    _ -> pure ()
handleEvent (MouseDown {}) = pure ()
handleEvent (MouseUp {}) = pure ()

notification :: ByteString
notification = $(embedFile =<< makeRelativeToProject "notification.wav")

playNotificationSound :: IO ()
playNotificationSound = void $ async $ runProcess process
  where
    process =
      shell "aplay"
        & setStdin (byteStringInput $ fromStrict notification)
        & setStdout nullStream
        & setStderr nullStream
