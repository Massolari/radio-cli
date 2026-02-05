module RadioCLI.Player where

import Control.Concurrent qualified
import Control.Concurrent.MVar qualified
import Control.Exception qualified
import Control.Monad qualified
import Data.Char qualified
import Data.Foldable (find)
import Data.List qualified
import Data.String qualified
import GHC.IO.Handle qualified
import System.IO qualified
import System.IO.Error qualified
import System.Process qualified

data Player = Player
  { playerStdin :: GHC.IO.Handle.Handle,
    playerStdout :: GHC.IO.Handle.Handle,
    playerIsPlaying :: Bool,
    playerVolume :: Int
  }

data Metadata = Metadata
  { nowPlaying :: Maybe String,
    artist :: Maybe String,
    title :: Maybe String
  }
  deriving (Show, Eq)

data CurrentlyPlaying
  = PlayingLoading Int
  | PlayingSong String
  | PlayingStopped
  | PlayingUnknown

-- | Creates a new Player instance that streams audio from the given URL.
-- The player uses VLC with the RC (remote control) interface to enable
-- programmatic control and status querying.
new :: String -> IO Player
new stream = do
  (Just handlerStdin, Just handlerStdout, _, _) <-
    System.Process.createProcess
      (System.Process.proc "vlc" ["-I", "rc", stream])
        { System.Process.std_in = System.Process.CreatePipe,
          System.Process.std_out = System.Process.CreatePipe,
          System.Process.std_err = System.Process.NoStream
        }

  GHC.IO.Handle.hSetBuffering handlerStdin GHC.IO.Handle.NoBuffering
  GHC.IO.Handle.hSetBuffering handlerStdout GHC.IO.Handle.NoBuffering

  System.IO.hPutStrLn handlerStdin "volume 300"

  return
    Player
      { playerStdin = handlerStdin,
        playerStdout = handlerStdout,
        playerIsPlaying = True,
        playerVolume = 100
      }

-- | Retrieves the currently playing track information from the player.
-- Returns the cached value from the background update thread.
getCurrentPlaying :: Player -> CurrentlyPlaying -> IO CurrentlyPlaying
getCurrentPlaying (Player {playerIsPlaying = False}) _ = return PlayingStopped
getCurrentPlaying player current = do
  System.IO.hPutStrLn (playerStdin player) "info"
  System.IO.hFlush (playerStdin player)

  hasInput <-
    System.IO.Error.catchIOError
      (System.IO.hWaitForInput (playerStdout player) 500)
      (\_ -> return False)

  if hasInput
    then getCurrentPlayingFromOutput player current
    else return $ PlayingLoading 0

-- | Reads and parses the player's output to extract the now playing information.
-- Recursively reads lines from stdout until end of info block or no more input available.
-- Returns the last found now_playing value, or Nothing if not found.
getCurrentPlayingFromOutput :: Player -> CurrentlyPlaying -> IO CurrentlyPlaying
getCurrentPlayingFromOutput player current = do
  metadata <- getCurrentPlayingFromOutput' player $ Metadata {nowPlaying = Nothing, artist = Nothing, title = Nothing}
  return $ case nowPlaying metadata of
    Just np -> PlayingSong np
    Nothing ->
      case (artist metadata, title metadata) of
        (Just a, Just t) -> PlayingSong (a ++ " - " ++ t)
        (Just a, Nothing) -> PlayingSong a
        (Nothing, Just t) -> PlayingSong t
        (Nothing, Nothing) -> case current of
          PlayingLoading count | count < 6 -> PlayingLoading (count + 1)
          PlayingLoading _ -> PlayingUnknown
          PlayingSong _ -> PlayingLoading 0
          PlayingStopped -> PlayingLoading 0
          PlayingUnknown -> PlayingUnknown
  where
    handlers :: [(String -> Bool, String -> Metadata -> Metadata)]
    handlers =
      [ ( ("| now_playing: " `Data.List.isPrefixOf`),
          \line metadata -> metadata {nowPlaying = Just (trim $ drop (length "| now_playing: ") line)}
        ),
        ( ("| artist: " `Data.List.isPrefixOf`),
          \line metadata -> metadata {artist = Just (trim $ drop (length "| artist: ") line)}
        ),
        ( ("| title: " `Data.List.isPrefixOf`),
          \line metadata -> metadata {title = Just (trim $ drop (length "| title: ") line)}
        )
      ]

    handleLine :: String -> Metadata -> Metadata
    handleLine line metadata =
      foldl
        ( \md (handlerCondition, handlerFun) ->
            if handlerCondition line
              then
                handlerFun line md
              else md
        )
        metadata
        handlers

    getCurrentPlayingFromOutput' :: Player -> Metadata -> IO Metadata
    getCurrentPlayingFromOutput' player metadata = do
      ready <-
        System.IO.Error.catchIOError
          (System.IO.hReady (playerStdout player))
          (\_ -> return False)

      if not ready
        then return metadata
        else do
          line <-
            System.IO.Error.catchIOError
              (GHC.IO.Handle.hGetLine (playerStdout player))
              (\_ -> return "+----[ end of stream info ]")

          let trimmedLine = trim line

          if "+----[ end of stream info ]" `Data.List.isInfixOf` trimmedLine
            then return metadata
            else
              getCurrentPlayingFromOutput'
                player
                $ handleLine line metadata

-- | Removes leading and trailing whitespace from a string.
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile Data.Char.isSpace

play :: Player -> String -> IO Player
play player stream = do
  System.IO.hPutStrLn (playerStdin player) "clear"
  System.IO.hPutStrLn (playerStdin player) $ "add " ++ stream
  System.IO.hPutStrLn (playerStdin player) "play"

  return player {playerIsPlaying = True}

stop :: Player -> IO Player
stop player = do
  System.IO.hPutStrLn (playerStdin player) "stop"
  return player {playerIsPlaying = False}

resume :: Player -> IO Player
resume player = do
  System.IO.hPutStrLn (playerStdin player) "play"
  return player {playerIsPlaying = True}

resumeStop :: Player -> IO Player
resumeStop player@Player {playerIsPlaying = True} = stop player
resumeStop player@Player {playerIsPlaying = False} = resume player

isPlaying :: Player -> Bool
isPlaying = playerIsPlaying

volumeUp :: Player -> IO Player
volumeUp player = do
  let newVolume = min 100 (playerVolume player + 10)
  System.IO.hPutStrLn (playerStdin player) $ "volume " ++ show (newVolume * 3)
  return player {playerVolume = newVolume}

volumeDown :: Player -> IO Player
volumeDown player = do
  let newVolume = max 0 (playerVolume player - 10)
  System.IO.hPutStrLn (playerStdin player) $ "volume " ++ show (newVolume * 3)
  return player {playerVolume = newVolume}

getVolume :: Player -> Int
getVolume = playerVolume
