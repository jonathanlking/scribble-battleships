{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.BattleShips
  ( server
  , Location(..)
  ) where

import           Prelude
import           Control.Monad       (forever, (>=>), guard)
import           Data.Aeson          (Value, (.:), withObject, encode, decode, toJSON, parseJSON, tagSingleConstructors, defaultOptions, sumEncoding, (.=), genericParseJSON, object, SumEncoding(..), ToJSON, FromJSON)
import           Data.Monoid         ((<>))
import           Data.Maybe          (fromJust)
import           Data.Text           (Text)
import qualified Data.Text          as T
import           Data.String.Conversions (convertString)
import           GHC.Generics        (Generic)
import qualified Network.WebSockets as WS
import           Control.Lens        ((^?), (&), Prism')
import           Data.Aeson.Lens     (key, _String)
import           Control.Monad.Fix   (fix)
import           Control.Concurrent  (threadDelay)
import           System.Random       (newStdGen, randomRs)
import           Data.Aeson.Encoding.Scribble

newtype Location = Location Int
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via (ScribbleJSON Location)

newtype Config = Config Int
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via (ScribbleJSON Config)

data Attack = Attack Location
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via (ScribbleJSON Attack)

data Init = Init Config
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via (ScribbleJSON Init)

data Hit = Hit Location
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via (ScribbleJSON Hit)

data Miss = Miss Location
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via (ScribbleJSON Miss)

data Winner = Winner
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via (ScribbleJSON Winner)

data Loser = Loser
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via (ScribbleJSON Loser)

decodeOrDie bs 
  = case decode bs of
     Nothing -> error "can't be nothing"
     Just x  -> x 

-- This is an incredibly naive implementation
server :: WS.Connection -> IO ()
server conn = do
  (Init (Config p1Loc)) <- decodeOrDie <$> WS.receiveData conn
  g <- newStdGen
  let (p2Loc : rand) = randomRs @Int (0, 4) g
  loop p1Loc p2Loc [] rand
  where
    loop p1Loc p2Loc played ms = do
      print p1Loc
      print p2Loc
      print played
      (Attack (Location l)) <- decodeOrDie <$> WS.receiveData conn
      -- Simulate the other player taking time to choose their move
      threadDelay 2000000
      if l == p2Loc
      then WS.sendTextData conn $ encode $ Winner
      else do
        print "Miss"
        WS.sendTextData conn $ encode $ Miss $ Location l
        let (target : ms') = dropWhile ((flip elem) played) ms
        if target == p1Loc
        then do
          print "Loser"
          WS.sendTextData conn $ encode $ Loser
        else do
          print "They missed"
          WS.sendTextData conn $ encode $ Miss $ Location target
          loop p1Loc p2Loc (target : played) ms'
