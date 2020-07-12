{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Harvest where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Time.Calendar (Day, showGregorian)
import Network.HTTP.Req
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics

data TimeEntry = TimeEntry {
  hours :: Double
 } deriving (Generic, Show)

instance FromJSON TimeEntry

data TimeEntriesResponse = TimeEntriesResponse {
  time_entries :: [TimeEntry]
 } deriving (Generic, Show)

instance FromJSON TimeEntriesResponse

data HarvestIdentity = HarvestIdentity {
  accountId :: Text
, token :: Text
}

fetchDayTimeEntries :: MonadIO m => HarvestIdentity -> Day -> m [TimeEntry]
fetchDayTimeEntries identity day =
  let headers =
        header "Harvest-Account-Id" (encodeUtf8 $ accountId identity) <>
        header "Authorization" (encodeUtf8 $ "Bearer " <> token identity) <>
        header "User-Agent" "PostmanRuntime/7.25.0"
      queryParameters =
        "from" =: date day <>
        "to" =: date day
      options = headers <> queryParameters
  in do
    response <- runReq defaultHttpConfig $ req
      GET
      (https "api.harvestapp.com" /: "v2" /: "time_entries")
      NoReqBody
      jsonResponse
      options
    return . time_entries . responseBody $ response
  where
    date :: Day -> Text
    date = pack . showGregorian

sumHours :: [TimeEntry] -> Double
sumHours = sum . map hours
