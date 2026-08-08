{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI (runUI) where

import Brick
import qualified Brick.AttrMap as A
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Forms (allFieldsValid, formState, handleFormEvent)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Exception (finally)
import Data.Generics.Product
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Pomodoro
import Relude
import Relude.Extra.Lens
import Sound (playNotificationSound)
import UI.Style (attributes)
import UI.View

data View = Settings | Ready deriving (Eq)

data AppState = AppState
  { currentView :: View,
    pomodoro :: Pomodoro,
    settingsForm :: SettingsForm AppEvent
  }
  deriving (Generic)

data AppEvent = Tick deriving (Eq, Show)

uiApp :: App AppState AppEvent SettingsFormField
uiApp =
  App
    { appDraw = viewUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const $ A.attrMap V.defAttr attributes
    }

runUI :: IO ()
runUI = do
  let makeVty = mkVty V.defaultConfig
  vty <- makeVty
  bchan <- newBChan 1
  ticker <- async $ tickerLoop bchan
  let appState =
        AppState
          Ready
          (makePomodoro defaultSettings)
          (makePomodoroSettingsForm defaultSettings)
  void
    $ customMain vty makeVty (Just bchan) uiApp appState
    `finally` cancel ticker

tickerLoop :: BChan AppEvent -> IO ()
tickerLoop bchan = forever $ do
  threadDelay 1000000
  writeBChan bchan Tick

viewUI :: AppState -> [Widget SettingsFormField]
viewUI (AppState Settings _ form) = [viewPomodoroSettings form]
viewUI (AppState Ready pomodoro _) = [viewPomodoro pomodoro]

handleEvent ::
  BrickEvent SettingsFormField AppEvent -> EventM SettingsFormField AppState ()
handleEvent (VtyEvent (V.EvResize {})) = pure ()
handleEvent e@(VtyEvent (V.EvKey key [])) = case key of
  V.KChar ' ' -> zoom #pomodoro $ modify startOrPause
  V.KChar 'r' -> do
    gets getTyped >>= \case
      Settings -> do
        modify $ #pomodoro . #pomodoroSettings .~ defaultSettings
        modify $ #settingsForm .~ makePomodoroSettingsForm defaultSettings
      Ready -> zoom #pomodoro $ modify reset
  V.KChar 'q' -> halt
  V.KChar 's' -> do
    AppState v Pomodoro {..} _ <- get
    when (v == Ready) $ do
      modify $ #settingsForm .~ makePomodoroSettingsForm pomodoroSettings
      modify $ #currentView .~ Settings
  V.KEnter -> do
    AppState v Pomodoro {..} form <- get
    when (v == Settings && allFieldsValid form) $ do
      let newSettings = formState form
      modify $ #pomodoro . #pomodoroSettings .~ newSettings
      modify $ #currentView .~ Ready
      when (pomodoroState == Paused) $ do
        modify $ #pomodoro .~ makePomodoro newSettings
  V.KEsc -> halt
  _ -> forwardToForm e
handleEvent (AppEvent Tick) = do
  modify $ #pomodoro %~ tick
  phase <- gets $ view $ #pomodoro . #pomodoroPhase
  liftIO $ when (phase `elem` [Focus 0, Break 0]) playNotificationSound
handleEvent e@(MouseDown {}) = forwardToForm e
handleEvent e@(MouseUp {}) = forwardToForm e
handleEvent _ = pure ()

forwardToForm ::
  BrickEvent SettingsFormField AppEvent -> EventM SettingsFormField AppState ()
forwardToForm e = zoom #settingsForm $ handleFormEvent e
