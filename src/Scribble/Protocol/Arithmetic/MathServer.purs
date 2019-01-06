module Scribble.Protocol.Arithmetic.MathServer where

import Scribble.FSM
import Type.SList (type (:::), SLProxy(..), SNil, symbols)
import Type.Row (Cons, Nil)
import Data.Void (Void)

-- From purescript-argonaut-codecs
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Core (Json) -- From purescript-argonaut-core
import Data.Generic.Rep (class Generic) -- From purescript-generics-rep
-- From purescript-argonaut-generic
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)


data Add = Add Int Int
derive instance genericAdd :: Generic Add _
instance encodeJsonAdd :: EncodeJson Add where
  encodeJson = genericEncodeJson
instance decodeJsonAdd :: DecodeJson Add where
  decodeJson = genericDecodeJson
data Multiply = Multiply Int Int
derive instance genericMultiply :: Generic Multiply _
instance encodeJsonMultiply :: EncodeJson Multiply where
  encodeJson = genericEncodeJson
instance decodeJsonMultiply :: DecodeJson Multiply where
  decodeJson = genericDecodeJson
data Quit = Quit
derive instance genericQuit :: Generic Quit _
instance encodeJsonQuit :: EncodeJson Quit where
  encodeJson = genericEncodeJson
instance decodeJsonQuit :: DecodeJson Quit where
  decodeJson = genericDecodeJson
data Product = Product Int
derive instance genericProduct :: Generic Product _
instance encodeJsonProduct :: EncodeJson Product where
  encodeJson = genericEncodeJson
instance decodeJsonProduct :: DecodeJson Product where
  decodeJson = genericDecodeJson
data Sum = Sum Int
derive instance genericSum :: Generic Sum _
instance encodeJsonSum :: EncodeJson Sum where
  encodeJson = genericEncodeJson
instance decodeJsonSum :: DecodeJson Sum where
  decodeJson = genericDecodeJson

foreign import data MathServer :: Protocol

instance protocolNameMathServer :: ProtocolName MathServer "MathServer"

instance protocolRoleNamesMathServer :: ProtocolRoleNames MathServer ("Client" ::: "Server" ::: SNil)

foreign import data Client :: Role

instance roleNameClient :: RoleName Client "Client"

foreign import data S9Connect :: Type
foreign import data S9 :: Type
foreign import data S9Add :: Type
foreign import data S9Multiply :: Type
foreign import data S9Quit :: Type
foreign import data S9Disconnect :: Type
foreign import data S10 :: Type
foreign import data S11 :: Type
foreign import data S12 :: Type

instance initialClient :: Initial Client S9Connect
instance connectS9Connect :: Connect Client Server S9Connect S9
instance terminalClient :: Terminal Client S10
instance sendS9Add :: Send Server S9Add S11 Add
instance sendS9Multiply :: Send Server S9Multiply S12 Multiply
instance disconnectS9Disconnect :: Disconnect Client Server S9Disconnect S10
instance sendS9Quit :: Send Server S9Quit S9Disconnect Quit
instance selectS9 :: Select Server S9 (Cons "multiply" S9Multiply (Cons "quit" S9Quit (Cons "add" S9Add Nil)))
instance receiveS11 :: Receive Server S11 S9 Sum
-- instance sendS11 ::
--      Fail "You can only receive in this state"
--   => Send r S11 t a
instance receiveS12 :: Receive Server S12 S9 Product

foreign import data Server :: Role

instance roleNameServer :: RoleName Server "Server"

foreign import data S20 :: Type
foreign import data S20Add :: Type
foreign import data S20Multiply :: Type
foreign import data S20Quit :: Type
foreign import data S21 :: Type
foreign import data S22 :: Type
foreign import data S23 :: Type

instance initialServer :: Initial Server S20
instance terminalServer :: Terminal Server S21
instance receiveS20Add :: Receive Client S20Add S22 Add
instance receiveS20Multiply :: Receive Client S20Multiply S23 Multiply
instance receiveS20Quit :: Receive Client S20Quit S21 Quit
instance branchS20 :: Branch Server S20 (Cons "add" S20Add (Cons "quit" S20Quit (Cons "multiply" S20Multiply Nil)))
instance sendS22 :: Send Client S22 S20 Sum
instance sendS23 :: Send Client S23 S20 Product

