{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

type Seconds = Int

data PomodoroConfig = PomodoroConfig
  { focusDuration :: Seconds,
    shortBreakDuration :: Seconds,
    longBreakDuration :: Seconds
  }
  deriving (Generic)

defaultConfig :: PomodoroConfig
defaultConfig = PomodoroConfig 25 5 10

--------------------------------------------------------------------------------

data Pomodoro = Pomodoro
  { pomodoroPhase :: PomodoroPhase,
    pomodoroState :: PomodoroState,
    pomodoroCompletedRuns :: Int
  }
  deriving (Generic, Eq)

data PomodoroPhase = Focus Seconds | Break Seconds deriving (Generic, Eq, Ord)

data PomodoroState = Running | Paused deriving (Generic, Eq, Ord)

makePomodoro :: PomodoroConfig -> Pomodoro
makePomodoro PomodoroConfig {..} = Pomodoro (Focus focusDuration) Paused 0

startOrPause :: Pomodoro -> Pomodoro
startOrPause = #pomodoroState %~ toggleState

reset :: PomodoroConfig -> Pomodoro -> Pomodoro
reset PomodoroConfig {..} =
  (#pomodoroState .~ Paused) . (#pomodoroPhase .~ Focus focusDuration)

tick :: PomodoroConfig -> Pomodoro -> Pomodoro
tick _ p@(Pomodoro _ Paused _) = p
tick PomodoroConfig {..} p@Pomodoro {..} = case pomodoroPhase of
  Focus 0 ->
    let longBreak = Break longBreakDuration
        shortBreak = Break shortBreakDuration
        isCycleCompleted = (pomodoroCompletedRuns + 1) `mod` 4 == 0
        newPhase = if isCycleCompleted then longBreak else shortBreak
     in p & #pomodoroPhase .~ newPhase & #pomodoroCompletedRuns %~ (+ 1)
  Break 0 -> p {pomodoroPhase = Focus focusDuration}
  Focus s -> p {pomodoroPhase = Focus $ s - 1}
  Break s -> p {pomodoroPhase = Break $ s - 1}

toggleState :: PomodoroState -> PomodoroState
toggleState Running = Paused
toggleState Paused = Running
