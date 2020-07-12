{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Time.Calendar (Day)
import Data.Time.LocalTime (getZonedTime, localDay, zonedTimeToLocalTime)
import Data.Text (pack)
import qualified Macos.Notification as N
import Harvest (HarvestIdentity(..), fetchDayTimeEntries, sumHours)
import qualified Overtime as O (notification)
import System.Environment (getArgs)

today :: IO Day
today = fmap (localDay . zonedTimeToLocalTime) getZonedTime

notify :: HarvestIdentity -> Day -> IO ()
notify identity day = do
  timeEntries <- fetchDayTimeEntries identity day
  let hours = sumHours timeEntries
      notification = O.notification hours
  case notification of
    Nothing -> return ()
    Just n -> N.notify n

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
  then putStrLn "usage: accountId token"
  else do
    let (account:token:_) = args
        identity = HarvestIdentity (pack account) (pack token)
    day <- today
    notify identity day
