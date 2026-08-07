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

viewPomodoro :: Pomodoro -> Widget a
viewPomodoro (Pomodoro {..}) = center $ vBox $ do
  hCenter
    <$> [ withAttr (if pomodoroState == Paused then titleAttr else phaseAttr)
            $ txt phaseText,
          padAll 1 $ vBox $ do
            hCenter
              <$> [ withAttr (if pomodoroState == Paused then titleAttr else phaseAttr) $ viewTimer secondsRemaining,
                    txt $ "Pomodoros completed: " <> show pomodoroCompletedRuns,
                    txt " "
                  ],
          withAttr helpAttr $ hBox $ padLeftRight 2 . txt <$> keys
        ]
  where
    phaseAttr = phaseAttrName pomodoroPhase
    (phaseText, secondsRemaining) = case pomodoroPhase of
      Focus s -> ("FOCUS", s)
      Break s -> ("BREAK", s)
    keys = [keySpace, keyR, keyS, keyQ]
    keySpace = "[Space] " <> spaceActionText
    keyR = "[R] reset"
    keyS = "[S] settings"
    keyQ = "[Q] quit"
    spaceActionText = if pomodoroState == Paused then "start" else "pause"

viewPomodoroConfig ::
  Form PomodoroConfig e ConfigFormField -> Widget ConfigFormField
viewPomodoroConfig form = center $ vBox $ do
  hCenter
    <$> [ withAttr titleAttr $ txt "POMODORO SETTINGS",
          padTop (Pad 2) $ padBottom (Pad 1) $ hBox [renderForm form],
          withAttr helpAttr $ hBox $ padLeftRight 2 . txt <$> keys
        ]
  where
    keys = ["[Tab] next field", "[Enter] save"]

data ConfigFormField
  = FocusDurationField
  | ShortBreakDurationField
  | LongBreakDurationField
  | CyclesForLongBreakField
  deriving (Eq, Ord, Show)

makePomodoroConfigForm :: PomodoroConfig -> Form PomodoroConfig e ConfigFormField
makePomodoroConfigForm = newForm fields
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
