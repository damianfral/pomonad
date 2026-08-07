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

data PomodoroConfig = PomodoroConfig
  { focusDuration :: Minutes,
    shortBreakDuration :: Minutes,
    longBreakDuration :: Minutes,
    cyclesForLongBreak :: Int
  }
  deriving (Generic, Eq)

defaultConfig :: PomodoroConfig
defaultConfig = PomodoroConfig 25 5 15 4

minutesToSeconds :: Minutes -> Seconds
minutesToSeconds (Minutes m) = Seconds $ 60 * m

--------------------------------------------------------------------------------

data Pomodoro = Pomodoro
  { pomodoroPhase :: PomodoroPhase,
    pomodoroState :: PomodoroState,
    pomodoroCompletedRuns :: Int,
    pomodoroConfig :: PomodoroConfig
  }
  deriving (Generic, Eq)

data PomodoroPhase = Focus Seconds | Break Seconds deriving (Generic, Eq, Ord)

data PomodoroState = Running | Paused deriving (Generic, Eq, Ord)

makePomodoro :: PomodoroConfig -> Pomodoro
makePomodoro config@PomodoroConfig {..} =
  Pomodoro (Focus $ minutesToSeconds focusDuration) Paused 0 config

startOrPause :: Pomodoro -> Pomodoro
startOrPause = #pomodoroState %~ toggleState

reset :: Pomodoro -> Pomodoro
reset p@Pomodoro {pomodoroConfig = PomodoroConfig {..}} =
  let seconds = minutesToSeconds focusDuration
   in p & #pomodoroState .~ Paused & #pomodoroPhase .~ Focus seconds

tick :: Pomodoro -> Pomodoro
tick p@(Pomodoro _ Paused _ _) = p
tick p@Pomodoro {pomodoroConfig = PomodoroConfig {..}, ..} =
  case pomodoroPhase of
    Focus 0 ->
      let shortBreak = Break $ minutesToSeconds shortBreakDuration
          longBreak = Break $ minutesToSeconds longBreakDuration
          isCycleCompleted =
            (pomodoroCompletedRuns + 1) `mod` cyclesForLongBreak == 0
          newPhase = if isCycleCompleted then longBreak else shortBreak
       in p & #pomodoroPhase .~ newPhase & #pomodoroCompletedRuns %~ (+ 1)
    Break 0 -> p {pomodoroPhase = Focus $ minutesToSeconds focusDuration}
    Focus s -> p {pomodoroPhase = Focus $ s - 1}
    Break s -> p {pomodoroPhase = Break $ s - 1}

toggleState :: PomodoroState -> PomodoroState
toggleState Running = Paused
toggleState Paused = Running

--------------------------------------------------------------------------------
