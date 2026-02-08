module Main where

import Options.Applicative
import Lazuli.Server.Server (startServer)

data ServerOpts = ServerOpts
  { optPort :: Int
  }

serverOpts :: Parser ServerOpts
serverOpts = ServerOpts
  <$> option auto
      ( long "port"
      <> short 'p'
      <> metavar "PORT"
      <> value 3000
      <> showDefault
      <> help "Port to run the server on"
      )

main :: IO ()
main = do
  opts <- execParser optsInfo
  startServer (optPort opts)
  where
    optsInfo = info (serverOpts <**> helper)
      ( fullDesc
      <> progDesc "Lazuli Web UI Server"
      <> header "lazuli-server - Web interface for Lazuli wallpaper generator"
      )
