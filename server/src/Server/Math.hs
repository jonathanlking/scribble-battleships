{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Server.Math
  ( server
  ) where

import           Prelude
import           Control.Monad       (forever, (>=>))
import           Data.Aeson          (encode, decode, toJSON, parseJSON, tagSingleConstructors, unwrapUnaryRecords, defaultOptions, sumEncoding, (.=), genericParseJSON, object, SumEncoding(..), ToJSON, FromJSON, genericToJSON)
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
import           Data.Aeson.Encoding.Scribble

data Add = Add Int Int
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via (ScribbleJSON Add)

data Multiply = Multiply Int Int
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via (ScribbleJSON Multiply)

data Sum = Sum Int
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via (ScribbleJSON Sum)

data Product = Product Int
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via (ScribbleJSON Product)

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
