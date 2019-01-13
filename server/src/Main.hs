{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude
import           Control.Monad ((>=>))
import qualified Network.WebSockets as WS
import           Control.Concurrent (forkIO)

import           Server.Math as M
import           Server.BattleShips as BS

main :: IO ()
main = do
  putStrLn $ "Listening on " <> host <> ":" <> show wsPort
  forkIO $ WS.runServer host wsPort (WS.acceptRequest >=> M.server)
  WS.runServer host 9161 (WS.acceptRequest >=> BS.server)
  where
    host = "localhost"
    wsPort = 9160
