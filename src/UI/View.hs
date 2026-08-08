{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.View where

import Brick
import Brick.Forms
import Brick.Widgets.Center (center, hCenter)
import Data.Generics.Labels ()
import Pomodoro
import Relude
import UI.Style
import UI.Typography

viewTimer :: Seconds -> Widget n
viewTimer s = hBox $ padAll 1 . hLimit 5 . hCenter . vBox <$> digits
  where
    digits =
      [ [txt $ renderDigit minuteTens],
        [txt $ renderDigit minuteOnes],
        [txt dots],
        [txt $ renderDigit secondTens],
        [txt $ renderDigit secondOnes]
      ]
    minutes = unSeconds s `div` 60
    seconds = unSeconds s `mod` 60
    minuteTens = minutes `div` 10
    minuteOnes = minutes `mod` 10
    secondTens = seconds `div` 10
    secondOnes = seconds `mod` 10

viewScreen :: Widget n -> Widget n -> [Text] -> Widget n
viewScreen header body keys =
  center $ vBox $ hCenter <$> [header, body, help]
  where
    help = withAttr helpAttr $ hBox $ padLeftRight 2 . txt <$> keys

viewPomodoro :: Pomodoro -> Widget a
viewPomodoro (Pomodoro {..}) = viewScreen header body keys
  where
    attr = if pomodoroState == Paused then titleAttr else phaseAttrName pomodoroPhase
    header = withAttr attr $ txt phaseText
    body =
      padAll 1
        $ vBox
        $ hCenter
        <$> [ withAttr attr $ viewTimer secondsRemaining,
              txt $ "Pomodoros completed: " <> show pomodoroCompletedRuns,
              txt " "
            ]
    keys = [keySpace, keyR, keyS, keyQ]
    keySpace = "[Space] " <> spaceActionText
    keyR = "[R] reset"
    keyS = "[S] settings"
    keyQ = "[Q] quit"
    spaceActionText = if pomodoroState == Paused then "start" else "pause"
    (phaseText, secondsRemaining) = case pomodoroPhase of
      Focus s -> ("FOCUS", s)
      Break s -> ("BREAK", s)

viewPomodoroSettings :: SettingsForm e -> Widget SettingsFormField
viewPomodoroSettings form = viewScreen header body keys
  where
    header = withAttr titleAttr $ txt "POMODORO SETTINGS"
    body = padTop (Pad 2) $ padBottom (Pad 1) $ hBox [renderForm form]
    keys = ["[Tab] next field", "[R] Reset to default", "[Enter] save"]

data SettingsFormField
  = FocusDurationField
  | ShortBreakDurationField
  | LongBreakDurationField
  | CyclesForLongBreakField
  deriving (Eq, Ord, Show)

type SettingsForm e = Form PomodoroSettings e SettingsFormField

makePomodoroSettingsForm :: PomodoroSettings -> SettingsForm e
makePomodoroSettingsForm = newForm fields
  where
    label s w = padBottom (Pad 1) $ vLimit 1 $ hLimit 32 $ do
      hBox [hLimit 24 $ fill ' ' <+> str (s <> ": "), hLimit 8 w]
    focusDurationField =
      label "focus duration"
        @@= editShowableField #focusDuration FocusDurationField
    shortBreakDurationField =
      label "short break"
        @@= editShowableField #shortBreakDuration ShortBreakDurationField
    longBreakDurationField =
      label "long break"
        @@= editShowableField #longBreakDuration LongBreakDurationField
    cyclesForLongBreakField =
      label "cycles for long break"
        @@= editShowableField #cyclesForLongBreak CyclesForLongBreakField
    fields =
      [ focusDurationField,
        shortBreakDurationField,
        longBreakDurationField,
        cyclesForLongBreakField
      ]
