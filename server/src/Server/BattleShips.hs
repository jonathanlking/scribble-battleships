{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.BattleShips
  ( server
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

argonautOptions = defaultOptions {tagSingleConstructors = True, sumEncoding = TaggedObject "tag" "values"}

-- TODO: Make generic JSON representation compatible

newtype Location = Location Int
  deriving (Generic, Show)
instance FromJSON Location where
  parseJSON 
    = withObject "Location" $ \v -> do
        tag <- v .: "tag"
        guard (tag == ("Location" :: Text))
        vs <- v .: "values"
        case vs of
          [(x :: Int)] -> pure $ Location x
          _ -> mempty
instance ToJSON Location where
  toJSON (Location l) =
      object ["tag" .= ("Location" :: Text), "values" .= [toJSON l]]

newtype Config = Config Int
  deriving (Generic, Show)
instance FromJSON Config where
  parseJSON 
    = withObject "Config" $ \v -> do
        tag <- v .: "tag"
        guard (tag == ("Config" :: Text))
        vs <- v .: "values"
        case vs of
          [(x :: Int)] -> pure $ Config x
          _ -> mempty
instance ToJSON Config where
  toJSON (Config loc) =
      object ["tag" .= ("Config" :: Text), "values" .= [toJSON loc]]

data Attack = Attack Location
  deriving (Generic, Show)
instance ToJSON Attack where
  toJSON (Attack loc) =
      object ["tag" .= ("Attack" :: Text), "values" .= [toJSON loc]]
instance FromJSON Attack where
  parseJSON 
    = withObject "Attack" $ \v -> do
        tag <- v .: "tag"
        guard (tag == ("Attack" :: Text))
        vs <- v .: "values"
        case vs of
          [(x :: Location)] -> pure $ Attack x
          _ -> mempty

data Init = Init Config
  deriving (Generic, Show)
instance FromJSON Init where
  parseJSON 
    = withObject "Init" $ \v -> do
        tag <- v .: "tag"
        guard (tag == ("Init" :: Text))
        vs <- v .: "values"
        case vs of
          [(x :: Config)] -> pure $ Init x
          _ -> mempty
instance ToJSON Init where
  toJSON (Init config) =
      object ["tag" .= ("Init" :: Text), "values" .= [toJSON config]]


data Hit = Hit Location
  deriving (Generic, Show)
instance ToJSON Hit where
  toJSON (Hit loc) =
      object ["tag" .= ("Hit" :: Text), "values" .= [toJSON loc]]
instance FromJSON Hit where
  parseJSON 
    = withObject "Hit" $ \v -> do
        tag <- v .: "tag"
        guard (tag == ("Hit" :: Text))
        vs <- v .: "values"
        case vs of
          [(x :: Location)] -> pure $ Hit x
          _ -> mempty

data Miss = Miss Location
  deriving (Generic, Show)
instance ToJSON Miss where
  toJSON (Miss loc) =
      object ["tag" .= ("Miss" :: Text), "values" .= [toJSON loc]]
instance FromJSON Miss where
  parseJSON 
    = withObject "Miss" $ \v -> do
        tag <- v .: "tag"
        guard (tag == ("Miss" :: Text))
        vs <- v .: "values"
        case vs of
          [(x :: Location)] -> pure $ Miss x
          _ -> mempty

data Winner = Winner
  deriving (Generic, Show)
instance ToJSON Winner where
  toJSON Winner =
      object ["tag" .= ("Winner" :: Text), "values" .= ([] :: [Value])]

data Loser = Loser
  deriving (Generic, Show)
instance ToJSON Loser where
  toJSON Loser =
      object ["tag" .= ("Loser" :: Text), "values" .= ([] :: [Value])]


data Add = Add Int Int
  deriving (Generic, Show)
instance FromJSON Add where
  parseJSON = genericParseJSON argonautOptions

data Multiply = Multiply Int Int
  deriving (Generic, Show)
instance FromJSON Multiply where
  parseJSON = genericParseJSON argonautOptions

data Sum = Sum Int
  deriving (Generic, Show)
instance ToJSON Sum where
  toJSON (Sum res) =
      object ["tag" .= ("Sum" :: Text), "values" .= [res]]

data Product = Product Int
  deriving (Generic, Show)
instance ToJSON Product where
  toJSON (Product res) =
      object ["tag" .= ("Product" :: Text), "values" .= [res]]

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
          print "Looser"
          WS.sendTextData conn $ encode $ Loser
        else do
          print "They missed"
          WS.sendTextData conn $ encode $ Miss $ Location target
          loop p1Loc p2Loc (target : played) ms'
