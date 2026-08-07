{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.View where

import Brick
import Brick.Widgets.Center (center, hCenter)
import Pomodoro
import Relude
import UI.Style
import UI.Typography

viewTimer :: Int -> Widget n
viewTimer s = hBox $ padAll 1 . hLimit 5 . hCenter . vBox <$> digits
  where
    digits =
      [ [txt $ renderDigit minuteTens],
        [txt $ renderDigit minuteOnes],
        [txt dots],
        [txt $ renderDigit secondTens],
        [txt $ renderDigit secondOnes]
      ]
    minutes = s `div` 60
    seconds = s `mod` 60
    minuteTens = minutes `div` 10
    minuteOnes = minutes `mod` 10
    secondTens = seconds `div` 10
    secondOnes = seconds `mod` 10

viewPomodoro :: Pomodoro -> [Widget ()]
viewPomodoro (Pomodoro {..}) =
  [ center $ vBox $ do
      centerAndPad
        <$> [ withAttr phaseAttr $ txt phaseText,
              withAttr phaseAttr $ viewTimer secondsRemaining,
              txt $ "Pomodoros completed: " <> show pomodoroCompletedRuns,
              withAttr helpAttr $ do
                hBox $ padLeftRight 4 . txt <$> [keySpace, keyR, keyQ]
            ]
  ]
  where
    centerAndPad = hCenter . padAll 1
    phaseAttr = phaseAttrName pomodoroPhase
    (phaseText, secondsRemaining) = case pomodoroPhase of
      Focus s -> ("FOCUS", s)
      Break s -> ("BREAK", s)
    keySpace = "[Space] " <> spaceActionText
    keyR = "[R] reset"
    keyQ = "[Q] quit"
    spaceActionText = if pomodoroState == Paused then "start" else "pause"
