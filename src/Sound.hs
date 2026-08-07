{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sound where

import Control.Concurrent.Async (async)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Relude
import System.Process.Typed

notificationSound :: ByteString
notificationSound = $(embedFile =<< makeRelativeToProject "notification.wav")

playNotificationSound :: IO ()
playNotificationSound = void $ async $ runProcess process
  where
    process =
      shell "aplay"
        & setStdin (byteStringInput $ fromStrict notificationSound)
        & setStdout nullStream
        & setStderr nullStream
