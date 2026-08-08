{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Pomodoro where

import Data.Generics.Labels ()
import Relude
import Relude.Extra.Lens

--------------------------------------------------------------------------------

newtype Seconds = Seconds {unSeconds :: Int}
  deriving newtype (Num, Eq, Ord, Read, Show)

newtype Minutes = Minutes Int
  deriving newtype (Num, Eq, Ord, Read, Show)

data PomodoroSettings = PomodoroSettings
  { focusDuration :: Minutes,
    shortBreakDuration :: Minutes,
    longBreakDuration :: Minutes,
    cyclesForLongBreak :: Int
  }
  deriving (Generic, Eq)

defaultSettings :: PomodoroSettings
defaultSettings = PomodoroSettings 25 5 15 4

minutesToSeconds :: Minutes -> Seconds
minutesToSeconds (Minutes m) = Seconds $ 60 * m

--------------------------------------------------------------------------------

data Pomodoro = Pomodoro
  { pomodoroPhase :: PomodoroPhase,
    pomodoroState :: PomodoroState,
    pomodoroCompletedRuns :: Int,
    pomodoroSettings :: PomodoroSettings
  }
  deriving (Generic, Eq)

data PomodoroPhase = Focus Seconds | Break Seconds deriving (Generic, Eq, Ord)

data PomodoroState = Running | Paused deriving (Generic, Eq, Ord)

makePomodoro :: PomodoroSettings -> Pomodoro
makePomodoro config@PomodoroSettings {..} =
  Pomodoro (Focus $ minutesToSeconds focusDuration) Paused 0 config

startOrPause :: Pomodoro -> Pomodoro
startOrPause = #pomodoroState %~ toggleState

reset :: Pomodoro -> Pomodoro
reset p@Pomodoro {pomodoroSettings = PomodoroSettings {..}} =
  let seconds = minutesToSeconds focusDuration
   in p & #pomodoroState .~ Paused & #pomodoroPhase .~ Focus seconds

tick :: Pomodoro -> Pomodoro
tick p@(Pomodoro _ Paused _ _) = p
tick p@Pomodoro {pomodoroSettings = PomodoroSettings {..}, ..} =
  case pomodoroPhase of
    Focus 0 -> p & #pomodoroPhase .~ nextBreak & #pomodoroCompletedRuns %~ succ
    Break 0 -> p & #pomodoroPhase .~ focusPhase
    _ -> p & #pomodoroPhase %~ decrement
  where
    focusPhase = Focus $ minutesToSeconds focusDuration
    nextBreak
      | cycleCompleted = Break $ minutesToSeconds longBreakDuration
      | otherwise = Break $ minutesToSeconds shortBreakDuration
    cycleCompleted = (pomodoroCompletedRuns + 1) `mod` cyclesForLongBreak == 0
    decrement (Focus s) = Focus $ s - 1
    decrement (Break s) = Break $ s - 1

toggleState :: PomodoroState -> PomodoroState
toggleState Running = Paused
toggleState Paused = Running

--------------------------------------------------------------------------------
