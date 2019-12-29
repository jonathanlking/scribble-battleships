{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}

module Server.Math
  ( server
  ) where

import           Prelude
import           Control.Monad       (forever)
import           Data.Aeson (ToJSON, FromJSON, decode, encode)
import           Data.Maybe          (fromJust)
import           Data.Text           (Text)
import           Data.String.Conversions (convertString)
import           GHC.Generics        (Generic)
import qualified Network.WebSockets as WS
import           Control.Lens        ((^?))
import           Data.Aeson.Lens     (key, _String)
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
server conn
  = WS.receiveData @Text conn -- Ignore the first message received
  >> forever loop
  where
    loop = do
      action <- WS.receiveData @Text conn
      threadDelay 1000000
      case action ^? key "tag" . _String :: Maybe Text of
        Just tag | tag == "Add"      -> add $ fromJust (decode $ convertString action)
                 | tag == "Multiply" -> multiply $ fromJust (decode $ convertString action)
                 | tag == "Quit"     -> WS.sendClose conn ("Quit!" :: Text)
        _        -> error "The impossible happened"
    add (Add x y) = WS.sendTextData conn $ encode $ Sum $ x + y
    multiply (Multiply x y) = WS.sendTextData conn $ encode $ Product $ x * y
