{-# LANGUAGE OverloadedStrings #-}

module RadioCLI.Station where

import Data.Aeson ((.=))
import Data.Aeson qualified
import GHC.Generics qualified
import System.Directory qualified
import System.Environment qualified
import System.IO qualified

data Station = Station
  { stationName :: String,
    stationStream :: String
  }
  deriving (GHC.Generics.Generic, Show, Eq)

instance Data.Aeson.ToJSON Station where
  toJSON (Station {stationName = name, stationStream = stream}) =
    Data.Aeson.object
      [ "name" .= name,
        "stream" .= stream
      ]

  toEncoding (Station {stationName = name, stationStream = stream}) =
    Data.Aeson.pairs
      ( "name" .= name
          <> "stream" .= stream
      )

instance Data.Aeson.FromJSON Station where
  parseJSON = Data.Aeson.withObject "Station" $ \v ->
    Station
      <$> v Data.Aeson..: "name"
      <*> v Data.Aeson..: "stream"

getConfigDir :: IO FilePath
getConfigDir = do
  xdgHome <- System.Environment.getEnv "XDG_CONFIG_HOME"
  return $ xdgHome ++ "/radio-cli"

getConfigPath :: IO FilePath
getConfigPath = do
  configDir <- getConfigDir
  return $ configDir ++ "/stations.json"

getFromConfig :: IO (Either String [Station])
getFromConfig = do
  configPath <- getConfigPath
  doesPathExist <- System.Directory.doesFileExist configPath
  if doesPathExist
    then Data.Aeson.eitherDecodeFileStrict configPath :: IO (Either String [Station])
    else return $ Left "Config file does not exist."

defaultStations :: [Station]
defaultStations =
  [ Station {stationName = "Christian Hits", stationStream = "http://listen.christianrock.net/stream/12/"},
    Station {stationName = "Christian Rock", stationStream = "http://listen.christianrock.net/stream/11/"}
  ]
