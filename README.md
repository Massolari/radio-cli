# radio-cli

A terminal-based radio player written in Haskell using the [brick](https://github.com/jtdaugherty/brick) library. It uses VLC to play audio streams.

## Features

- TUI interface for browsing and playing radio stations
- Configurable station list via JSON
- Volume control
- "Now Playing" information display
- Support for any stream URL supported by VLC (including YouTube)

## Prerequisites

- **VLC Media Player**: This application requires VLC to be installed on your system, as it uses the `cvlc` command for playback.
- **Haskell Toolchain**: `cabal` (or `stack`) to build the project.

## Installation & Build

Clone the repository and build using cabal:

```bash
git clone <repository-url>
cd radio-cli
cabal build
cabal run
```

## Configuration

The application looks for a configuration file at:
`$XDG_CONFIG_HOME/radio-cli/stations.json`

(On Linux/Mac this is usually `~/.config/radio-cli/stations.json`)

If the file does not exist, the application will offer to create it with default stations.

### Configuration Format

The `stations.json` file should contain a JSON array of station objects:

```json
[
  {
    "name": "Christian Hits",
    "stream": "http://listen.christianrock.net/stream/12/"
  },
  {
    "name": "Christian Rock",
    "stream": "http://listen.christianrock.net/stream/11/"
  },
  {
    "name": "Lo-Fi Beats",
    "stream": "https://www.youtube.com/watch?v=jfKfPfyJRdk"
  }
]
```

- `name`: The display name of the station.
- `stream`: The URL of the stream. This can be any URL supported by VLC.

## Keyboard Shortcuts

| Key | Action |
| --- | --- |
| `j` | Move cursor down |
| `k` | Move cursor up |
| `J` | Move cursor down and play selected station |
| `K` | Move cursor up and play selected station |
| `g` | Go to top of the list |
| `G` | Go to bottom of the list |
| `Enter` | Play selected station |
| `Space` | Resume / Stop playback |
| `h` | Decrease volume |
| `l` | Increase volume |
| `q` | Quit application |
