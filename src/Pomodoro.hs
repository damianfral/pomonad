{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Pomodoro where

import Relude

data Pomodoro = Pomodoro
  { pomodoroPhase :: PomodoroPhase,
    pomodoroState :: PomodoroState,
    pomodoroCompletedRuns :: Int
  }
  deriving (Eq, Show)

data PomodoroPhase = Focus Int | Break Int deriving (Eq, Ord, Show)

data PomodoroState = Running | Paused deriving (Eq, Ord, Show)

focusPhase, shortBreakPhase, longBreakPhase :: PomodoroPhase
focusPhase = Focus $ 25 * 60
shortBreakPhase = Break $ 5 * 60
longBreakPhase = Break 30

initialState :: Pomodoro
initialState = Pomodoro focusPhase Paused 0

startOrPause :: Pomodoro -> Pomodoro
startOrPause st = st {pomodoroState = toggleState $ pomodoroState st}

reset :: Pomodoro -> Pomodoro
reset st = st {pomodoroState = Paused, pomodoroPhase = focusPhase}

tick :: Pomodoro -> Pomodoro
tick p@Pomodoro {..} = case pomodoroState of
  Paused -> p
  Running -> case pomodoroPhase of
    Focus 0 ->
      p
        { pomodoroPhase =
            if (pomodoroCompletedRuns + 1) `mod` 4 == 0
              then longBreakPhase
              else shortBreakPhase,
          pomodoroCompletedRuns = pomodoroCompletedRuns + 1
        }
    Break 0 -> p {pomodoroPhase = focusPhase}
    Focus s -> p {pomodoroPhase = Focus $ s - 1}
    Break s -> p {pomodoroPhase = Break $ s - 1}

toggleState :: PomodoroState -> PomodoroState
toggleState Running = Paused
toggleState Paused = Running
