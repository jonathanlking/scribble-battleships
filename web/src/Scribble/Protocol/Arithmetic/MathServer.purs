module Scribble.Protocol.Arithmetic.MathServer where

import Scribble.FSM
import Type.Row (Cons, Nil)
import Data.Void (Void)
import Data.Tuple (Tuple)

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
data Connect = Connect
derive instance genericConnect :: Generic Connect _
instance encodeJsonConnect :: EncodeJson Connect where
  encodeJson = genericEncodeJson
instance decodeJsonConnect :: DecodeJson Connect where
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

foreign import data Client :: Role

instance roleNameClient :: RoleName Client "Client"

foreign import data S11 :: Type
foreign import data S11Connected :: Type
foreign import data S13 :: Type
foreign import data S13Add :: Type
foreign import data S13Multiply :: Type
foreign import data S13Quit :: Type
foreign import data S14 :: Type
foreign import data S15 :: Type
foreign import data S16 :: Type
foreign import data S12 :: Type

instance initialClient :: Initial Client S11
instance terminalClient :: Terminal Client S12
instance connectS11 :: Connect Client Server S11 S11Connected
instance sendS11 :: Send Server S11Connected S13 Connect
instance sendS13Add :: Send Server S13Add S14 Add
instance sendS13Multiply :: Send Server S13Multiply S15 Multiply
instance sendS13Quit :: Send Server S13Quit S16 Quit
instance selectS13 :: Select Server S13 (Cons "multiply" S13Multiply (Cons "quit" S13Quit (Cons "add" S13Add Nil)))
instance receiveS14 :: Receive Server S14 S13 Sum
instance receiveS15 :: Receive Server S15 S13 Product
instance disconnectS16 :: Disconnect Client Server S16 S12

foreign import data Server :: Role

instance roleNameServer :: RoleName Server "Server"

foreign import data S26 :: Type
foreign import data S28 :: Type
foreign import data S28Add :: Type
foreign import data S28Multiply :: Type
foreign import data S28Quit :: Type
foreign import data S29 :: Type
foreign import data S30 :: Type
foreign import data S31 :: Type
foreign import data S27 :: Type

instance initialServer :: Initial Server S26
instance terminalServer :: Terminal Server S27
instance acceptS26 :: Accept Server Client S26 S28
instance receiveS28Add :: Receive Client S28Add S29 Add
instance receiveS28Multiply :: Receive Client S28Multiply S30 Multiply
instance receiveS28Quit :: Receive Client S28Quit S31 Quit
instance branchS28 :: Branch Server Client S28 (Cons "multiply" S28Multiply (Cons "quit" S28Quit (Cons "add" S28Add Nil)))
instance sendS29 :: Send Client S29 S28 Sum
instance sendS30 :: Send Client S30 S28 Product
instance disconnectS31 :: Disconnect Server Client S31 S27

