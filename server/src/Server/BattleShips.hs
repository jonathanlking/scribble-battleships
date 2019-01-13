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
data Loser = Loser
  deriving (Generic, Show)
data Winner = Winner
  deriving (Generic, Show)

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

server :: WS.Connection -> IO ()
server conn = do
  (Init loc) <- decodeOrDie <$> WS.receiveData conn
  print loc
  (Attack p) <- decodeOrDie <$> WS.receiveData conn
  print p
  threadDelay 1000000
  WS.sendTextData conn $ encode $ Hit p
  threadDelay 1000000000
  WS.sendTextData conn $ encode $ Hit p
--  case action ^? key "tag" . _String :: Maybe Text of
--    Just tag | tag == "Add"      -> add $ fromJust (decode $ convertString action)
--             | tag == "Multiply" -> multiply $ fromJust (decode $ convertString action)
--             | tag == "Quit"     -> WS.sendClose conn ("Quit!" :: Text))
--  where
--    add (Add x y) = WS.sendTextData conn $ encode $ Sum $ x + y
--    multiply (Multiply x y) = WS.sendTextData conn $ encode $ Product $ x * y
