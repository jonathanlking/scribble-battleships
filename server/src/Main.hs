module Main where

import           Prelude
import           Control.Monad ((>=>))
import qualified Network.WebSockets as WS

import           Server.BattleShips as BS

main :: IO ()
main = do
  putStrLn $ "Listening on " <> host <> ":" <> show wsPort
  WS.runServer host wsPort (WS.acceptRequest >=> BS.server)
  where
    host = "localhost"
    wsPort = 9160
