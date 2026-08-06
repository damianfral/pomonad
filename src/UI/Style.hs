{-# LANGUAGE OverloadedStrings #-}

module UI.Style where

import Brick (AttrName, attrName)
import Graphics.Vty.Attributes as V
import Pomodoro

attributes :: [(AttrName, V.Attr)]
attributes =
  [ ( phaseAttrName (Focus 0),
      V.defAttr `V.withForeColor` V.brightGreen `V.withStyle` V.bold
    ),
    ( phaseAttrName (Break 0),
      V.defAttr `V.withForeColor` V.brightYellow `V.withStyle` V.bold
    ),
    (helpAttr, V.defAttr `V.withStyle` V.dim)
  ]

phaseAttrName :: PomodoroPhase -> AttrName
phaseAttrName (Focus _) = attrName "focus"
phaseAttrName (Break _) = attrName "break"

helpAttr :: AttrName
helpAttr = attrName "help"
