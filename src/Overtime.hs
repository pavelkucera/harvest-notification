{-# LANGUAGE OverloadedStrings #-}

module Overtime where

import Data.Text (Text, pack, justifyRight)
import Macos.Notification (Notification(..))

fullDay :: Double
fullDay = 7.4

formatTime :: Double -> Text
formatTime t =
  let (hours, minuteFraction) = properFraction t
      minutes = round $ minuteFraction * 60
  in number hours <> ":" <> number minutes
  where
    number :: Show a => a -> Text
    number = justifyRight 2 '0' . pack . show

overtime :: Double -> Notification
overtime time =
  Notification {
    text = "Overtime of " <> formatTime time <> "h"
  , title = Just "You're working overtime."
  }

almostOvertime :: Double -> Notification
almostOvertime time =
  Notification {
    text = formatTime time <> "h left"
  , title = Just "You're almost done for today!"
  , subtitle = Nothing
  }

notification :: Double -> Maybe Notification
notification time
  | time > fullDay = Just $ overtime (time - fullDay)
  | time > (fullDay - 0.5) = Just $ almostOvertime (fullDay - time)
  | otherwise = Nothing
