{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Macos.Notification where

import Data.Bool (not)
import Data.Char (Char)
import Data.Function ((.), ($))
import Data.List (filter)
import Data.Maybe (Maybe, maybe)
import Data.Monoid ((<>))
import Data.Text (Text, singleton, unpack, unwords, null, replace)
import System.IO (IO)
import System.Process (callCommand)

data Notification = Notification {
  text :: Text
, title :: Maybe Text
, subtitle :: Maybe Text
}

quoteWith :: Char -> Text -> Text
quoteWith q s =
  eChar <> escape s <> eChar
  where
    eChar = singleton q

    escape :: Text -> Text
    escape = replace eChar (eChar <> singleton '\\' <> eChar <> eChar)

script :: Notification -> Text
script notification =
  unwords . filter (not . null) $ [
    "display notification",
    quote (text notification),
    parameter "with title" (title notification),
    parameter "subtitle" (subtitle notification)
   ]
  where
    quote = quoteWith '"'

    parameter :: Text -> Maybe Text -> Text
    parameter name = maybe "" (((name <> " ") <>) . quote)

notify :: Notification -> IO ()
notify notification =
  let appleScript = unpack . quoteWith '\'' . script $ notification
  in callCommand $ "osascript -e " <> appleScript
