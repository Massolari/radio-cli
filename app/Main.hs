module Main where

import Brick (Widget, joinBorders, simpleMain, str, txt, withBorderStyle, (<+>))
import Brick.AttrMap qualified
import Brick.BChan qualified
import Brick.Main qualified
import Brick.Types qualified
import Brick.Util qualified
import Brick.Widgets.Border (border, vBorder)
import Brick.Widgets.Border qualified
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Brick.Widgets.Center qualified
import Brick.Widgets.Core qualified
import Brick.Widgets.List qualified
import Brick.Widgets.ProgressBar qualified
import Control.Concurrent qualified
import Control.Concurrent qualified as Control.Monad
import Control.Monad qualified
import Control.Monad.IO.Class qualified
import Control.Monad.State qualified
import Data.Aeson qualified
import Data.Aeson.Decoding.ByteString.Lazy qualified
import Data.ByteString qualified
import Data.Text qualified
import Data.Vector qualified
import Graphics.Vty qualified
import RadioCLI.Player qualified
import RadioCLI.Station qualified
import System.Directory qualified
import System.Environment qualified
import System.Exit qualified
import System.Process qualified

data State = State
  { stateStations :: Brick.Widgets.List.List () RadioCLI.Station.Station,
    stateCurrentStation :: RadioCLI.Station.Station,
    statePlayer :: RadioCLI.Player.Player,
    stateCurrentPlaying :: RadioCLI.Player.CurrentlyPlaying
  }

drawUI :: State -> [Widget ()]
drawUI state =
  let stationsList = Data.Vector.toList (Brick.Widgets.List.listElements $ stateStations state)
      -- Add 2 for the cursor and space
      stationsWidth = 2 + maximum (length . RadioCLI.Station.stationName <$> stationsList)
      stationsHeight = length stationsList
   in [ Brick.Widgets.Core.hBox
          [ Brick.Widgets.Border.borderWithLabel (str "Stations") $
              Brick.Widgets.Core.padAll 1 $
                Brick.Widgets.Core.hLimit stationsWidth $
                  Brick.Widgets.Core.vLimit stationsHeight $
                    Brick.Widgets.List.renderList (listDrawElement (stateCurrentStation state)) True $
                      stateStations state,
            drawNowPlaying state
          ]
      ]

drawNowPlaying :: State -> Widget ()
drawNowPlaying state =
  let stationName = Data.Text.pack $ RadioCLI.Station.stationName $ stateCurrentStation state

      (artist, song) =
        case stateCurrentPlaying state of
          RadioCLI.Player.PlayingLoading _ -> (stationName, Data.Text.pack "Loading")
          RadioCLI.Player.PlayingSong info ->
            let parts = Data.Text.splitOn (Data.Text.pack " - ") $ Data.Text.pack info
             in case parts of
                  [artist, song] -> (artist, song)
                  _ -> (Data.Text.pack "", Data.Text.pack info)
          RadioCLI.Player.PlayingStopped -> (stationName, Data.Text.pack "Stopped")
          RadioCLI.Player.PlayingUnknown -> (stationName, Data.Text.pack "No song information")
   in Brick.Widgets.Border.borderWithLabel (str "Now Playing") $
        Brick.Widgets.Core.padAll 1 $
          Brick.Widgets.Core.vBox
            [ Brick.Widgets.Center.hCenter $
                str $
                  if RadioCLI.Player.isPlaying (statePlayer state)
                    then
                      "\61516" -- 󰏤
                    else
                      "\61515", -- 󰐊
              Brick.Widgets.Center.hCenter $ Brick.Widgets.Core.withAttr (Brick.AttrMap.attrName "bold") $ txt song,
              Brick.Widgets.Center.hCenter $ txt artist,
              Brick.Widgets.Core.padTop (Brick.Widgets.Core.Pad 1) $
                Brick.Widgets.Core.hBox $
                  ( if RadioCLI.Player.getVolume (statePlayer state) == 0
                      then
                        [ Brick.Widgets.Core.str "\984927  ", -- 󰝟
                          Brick.Widgets.Core.str "\60928" -- 
                        ]
                      else
                        [ Brick.Widgets.Core.str "\984446  ", -- 󰕾
                          Brick.Widgets.Core.str "\60931" -- 
                        ]
                  )
                    <> [ Brick.Widgets.ProgressBar.customProgressBar
                           '\60932' -- 
                           '\60929' -- 
                           Nothing
                           (fromIntegral (RadioCLI.Player.getVolume (statePlayer state)) / 100.0),
                         if RadioCLI.Player.getVolume (statePlayer state) == 100
                           then Brick.Widgets.Core.str "\60933" -- 
                           else Brick.Widgets.Core.str "\60930" -- 
                       ]
            ]

listDrawElement :: RadioCLI.Station.Station -> Bool -> RadioCLI.Station.Station -> Widget ()
listDrawElement currentStation isSelected station =
  let cursor =
        if isSelected
          then ">"
          else " "
      withCurrentAttribute =
        if currentStation == station
          then
            Brick.Widgets.Core.withAttr $ Brick.AttrMap.attrName "currentStation"
          else id
   in Brick.Widgets.Core.hBox
        [ str $ cursor <> " ",
          withCurrentAttribute $ str $ RadioCLI.Station.stationName station
        ]

data CustomEvent = UpdateCurrentPlaying

appEvent :: Brick.Types.BrickEvent () CustomEvent -> Brick.Types.EventM () State ()
appEvent event =
  case event of
    Brick.Types.VtyEvent vtyEvent -> appVtyEvent vtyEvent
    Brick.Types.AppEvent UpdateCurrentPlaying -> do
      state <- Control.Monad.State.get
      currentPlaying <-
        Control.Monad.IO.Class.liftIO $
          RadioCLI.Player.getCurrentPlaying (statePlayer state) (stateCurrentPlaying state)
      Control.Monad.State.modify $ \s -> s {stateCurrentPlaying = currentPlaying}

appVtyEvent :: Graphics.Vty.Event -> Brick.Types.EventM () State ()
appVtyEvent vtyEvent =
  case vtyEvent of
    Graphics.Vty.EvKey (Graphics.Vty.KChar 'g') [] -> do
      state <- Control.Monad.State.get
      let newStations = Brick.Widgets.List.listMoveToBeginning $ stateStations state
      Control.Monad.State.modify $ \s -> s {stateStations = newStations}
    Graphics.Vty.EvKey (Graphics.Vty.KChar 'G') [] -> do
      state <- Control.Monad.State.get
      let newStations = Brick.Widgets.List.listMoveToEnd $ stateStations state
      Control.Monad.State.modify $ \s -> s {stateStations = newStations}
    Graphics.Vty.EvKey (Graphics.Vty.KChar 'h') [] -> do
      state <- Control.Monad.State.get
      newPlayer <- Control.Monad.IO.Class.liftIO $ RadioCLI.Player.volumeDown (statePlayer state)
      Control.Monad.State.modify $ \s -> s {statePlayer = newPlayer}
    Graphics.Vty.EvKey (Graphics.Vty.KChar 'l') [] -> do
      state <- Control.Monad.State.get
      newPlayer <- Control.Monad.IO.Class.liftIO $ RadioCLI.Player.volumeUp (statePlayer state)
      Control.Monad.State.modify $ \s -> s {statePlayer = newPlayer}
    Graphics.Vty.EvKey (Graphics.Vty.KChar 'j') [] -> do
      state <- Control.Monad.State.get
      let newStations = Brick.Widgets.List.listMoveDown $ stateStations state
      Control.Monad.State.modify $ \s -> s {stateStations = newStations}
    Graphics.Vty.EvKey (Graphics.Vty.KChar 'J') [] -> do
      state <- Control.Monad.State.get
      let newStations = Brick.Widgets.List.listMoveDown $ stateStations state
      let selectedStation = Brick.Widgets.List.listSelectedElement newStations
      newPlayer <- Control.Monad.IO.Class.liftIO $
        case selectedStation of
          Just (_, station) -> RadioCLI.Player.play (statePlayer state) (RadioCLI.Station.stationStream station)
          Nothing -> return (statePlayer state)
      Control.Monad.State.modify $ \s ->
        s
          { stateStations = newStations,
            statePlayer = newPlayer,
            stateCurrentStation = maybe (stateCurrentStation s) snd selectedStation
          }
    Graphics.Vty.EvKey (Graphics.Vty.KChar 'k') [] -> do
      state <- Control.Monad.State.get
      let newStations = Brick.Widgets.List.listMoveUp $ stateStations state
      Control.Monad.State.modify $ \s -> s {stateStations = newStations}
    Graphics.Vty.EvKey (Graphics.Vty.KChar 'K') [] -> do
      state <- Control.Monad.State.get
      let newStations = Brick.Widgets.List.listMoveUp $ stateStations state
      let selectedStation = Brick.Widgets.List.listSelectedElement newStations
      newPlayer <- Control.Monad.IO.Class.liftIO $
        case selectedStation of
          Just (_, station) -> RadioCLI.Player.play (statePlayer state) (RadioCLI.Station.stationStream station)
          Nothing -> return (statePlayer state)
      Control.Monad.State.modify $ \s ->
        s
          { stateStations = newStations,
            statePlayer = newPlayer,
            stateCurrentStation = maybe (stateCurrentStation s) snd selectedStation
          }
    Graphics.Vty.EvKey (Graphics.Vty.KChar 'q') [] -> Brick.Main.halt
    Graphics.Vty.EvKey Graphics.Vty.KEnter [] -> do
      state <- Control.Monad.State.get
      let selectedStation = Brick.Widgets.List.listSelectedElement $ stateStations state
      case selectedStation of
        Just (_, station) -> do
          newPlayer <-
            Control.Monad.IO.Class.liftIO $
              RadioCLI.Player.play (statePlayer state) (RadioCLI.Station.stationStream station)
          Control.Monad.State.modify $ \s ->
            s
              { stateCurrentStation = station,
                stateCurrentPlaying = RadioCLI.Player.PlayingLoading 0,
                statePlayer = newPlayer
              }
        Nothing -> return ()
    Graphics.Vty.EvKey (Graphics.Vty.KChar ' ') [] -> do
      state <- Control.Monad.State.get
      let player = statePlayer state
      newPlayer <- Control.Monad.IO.Class.liftIO $ RadioCLI.Player.resumeStop player
      Control.Monad.State.modify $ \s ->
        s
          { statePlayer = newPlayer,
            stateCurrentPlaying =
              if RadioCLI.Player.isPlaying newPlayer
                then RadioCLI.Player.PlayingLoading 0
                else RadioCLI.Player.PlayingStopped
          }
    _ -> return ()

attrMap :: Brick.AttrMap.AttrMap
attrMap =
  Brick.AttrMap.attrMap
    Graphics.Vty.defAttr
    [ (Brick.AttrMap.attrName "bold", Brick.Util.style Graphics.Vty.bold),
      (Brick.AttrMap.attrName "currentStation", Graphics.Vty.withStyle (Brick.Util.style Graphics.Vty.underline) Graphics.Vty.bold)
    ]

app :: Brick.Main.App State CustomEvent ()
app =
  Brick.Main.App
    { Brick.Main.appDraw = drawUI,
      Brick.Main.appChooseCursor = Brick.Main.neverShowCursor,
      Brick.Main.appHandleEvent = appEvent,
      Brick.Main.appStartEvent = return (),
      Brick.Main.appAttrMap = const attrMap
    }

initialState :: [RadioCLI.Station.Station] -> IO State
initialState stations = do
  let currentStation = head stations
  playerInstance <- RadioCLI.Player.new $ RadioCLI.Station.stationStream currentStation
  return
    State
      { stateStations =
          Brick.Widgets.List.list
            ()
            ( Data.Vector.fromList
                stations
            )
            1,
        stateCurrentStation = currentStation,
        statePlayer = playerInstance,
        stateCurrentPlaying = RadioCLI.Player.PlayingLoading 0
      }

createConfigFile :: IO ()
createConfigFile = do
  configPath <- RadioCLI.Station.getConfigPath
  configDir <- RadioCLI.Station.getConfigDir
  System.Directory.createDirectoryIfMissing True configDir
  Data.Aeson.encodeFile configPath RadioCLI.Station.defaultStations

  putStrLn $ "Created config file at " ++ configPath
  putStrLn "Press Enter to continue..."
  _ <- getLine
  return ()

handleConfigError :: String -> IO [RadioCLI.Station.Station]
handleConfigError errMsg = do
  configPath <- RadioCLI.Station.getConfigPath

  putStrLn $ "Could not load stations from " ++ configPath
  putStrLn $ "Error: " ++ errMsg
  putStrLn "[1] Continue with default stations"
  putStrLn "[2] Create a config file with default stations and continue"
  putStrLn "[3] Create a config file with default stations and exit"
  putStrLn "[4] Exit"
  putStrLn "Choose an option:"
  choice <- getLine
  case choice of
    "1" ->
      return RadioCLI.Station.defaultStations
    "2" -> do
      createConfigFile
      return RadioCLI.Station.defaultStations
    "3" -> do
      createConfigFile
      exit
    "4" -> exit
    _ -> handleConfigError errMsg
  where
    exit = do
      putStrLn "Exiting."
      System.Exit.exitSuccess

main :: IO ()
main = do
  eitherStations <- RadioCLI.Station.getFromConfig
  stations <-
    case eitherStations of
      Right sts -> return sts
      Left errMsg -> handleConfigError errMsg
  chan <- Brick.BChan.newBChan 10

  Control.Monad.void $ Control.Concurrent.forkIO $ Control.Monad.forever $ do
    Brick.BChan.writeBChan chan UpdateCurrentPlaying
    -- 1 second delay
    Control.Concurrent.threadDelay 1_000_000

  state <- initialState stations

  (_, vty) <- Brick.Main.customMainWithDefaultVty (Just chan) app state
  Graphics.Vty.shutdown vty
