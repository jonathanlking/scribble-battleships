{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude
import           Control.Monad       (forever, (>=>))
import           Data.Aeson          (encode, decode, toJSON, parseJSON, tagSingleConstructors, defaultOptions, sumEncoding, (.=), genericParseJSON, object, SumEncoding(..), ToJSON, FromJSON)
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

main :: IO ()
main = do
  putStrLn $ "Listening on " <> host <> ":" <> show wsPort
  WS.runServer host wsPort (WS.acceptRequest >=> server)
  where
    host = "localhost"
    wsPort = 9160

server :: WS.Connection -> IO ()
server conn = (WS.receiveData conn :: IO Text) >> (forever $ do
  action <- WS.receiveData conn :: IO Text
  threadDelay 1000000
  case action ^? key "tag" . _String :: Maybe Text of
    Just tag | tag == "Add"      -> add $ fromJust (decode $ convertString action)
             | tag == "Multiply" -> multiply $ fromJust (decode $ convertString action)
             | tag == "Quit"     -> WS.sendClose conn ("Quit!" :: Text))
  where
    add (Add x y) = WS.sendTextData conn $ encode $ Sum $ x + y
    multiply (Multiply x y) = WS.sendTextData conn $ encode $ Product $ x * y
