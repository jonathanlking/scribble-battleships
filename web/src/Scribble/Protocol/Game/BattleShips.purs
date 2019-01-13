module Scribble.Protocol.Game.BattleShips where

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

import Game.BattleShips (Config, Location)

data Init = Init Config
derive instance genericInit :: Generic Init _
instance encodeJsonInit :: EncodeJson Init where
  encodeJson = genericEncodeJson
instance decodeJsonInit :: DecodeJson Init where
  decodeJson = genericDecodeJson
data Hit = Hit Location
derive instance genericHit :: Generic Hit _
instance encodeJsonHit :: EncodeJson Hit where
  encodeJson = genericEncodeJson
instance decodeJsonHit :: DecodeJson Hit where
  decodeJson = genericDecodeJson
data Miss = Miss Location
derive instance genericMiss :: Generic Miss _
instance encodeJsonMiss :: EncodeJson Miss where
  encodeJson = genericEncodeJson
instance decodeJsonMiss :: DecodeJson Miss where
  decodeJson = genericDecodeJson
data Attack = Attack Location
derive instance genericAttack :: Generic Attack _
instance encodeJsonAttack :: EncodeJson Attack where
  encodeJson = genericEncodeJson
instance decodeJsonAttack :: DecodeJson Attack where
  decodeJson = genericDecodeJson
data Loser = Loser
derive instance genericLoser :: Generic Loser _
instance encodeJsonLoser :: EncodeJson Loser where
  encodeJson = genericEncodeJson
instance decodeJsonLoser :: DecodeJson Loser where
  decodeJson = genericDecodeJson
data Winner = Winner
derive instance genericWinner :: Generic Winner _
instance encodeJsonWinner :: EncodeJson Winner where
  encodeJson = genericEncodeJson
instance decodeJsonWinner :: DecodeJson Winner where
  decodeJson = genericDecodeJson

foreign import data BattleShips :: Protocol

foreign import data P2 :: Role

instance roleNameP2 :: RoleName P2 "P2"

foreign import data S34 :: Type
foreign import data S34Connected :: Type
foreign import data S36 :: Type
foreign import data S36Hit :: Type
foreign import data S36Miss :: Type
foreign import data S36Loser :: Type
foreign import data S37 :: Type
foreign import data S39 :: Type
foreign import data S40 :: Type
foreign import data S35 :: Type
foreign import data S38 :: Type
foreign import data S38Hit :: Type
foreign import data S38Miss :: Type
foreign import data S38Winner :: Type
foreign import data S41 :: Type
foreign import data S41Hit :: Type
foreign import data S41Miss :: Type
foreign import data S41Winner :: Type

instance initialP2 :: Initial P2 S34
instance terminalP2 :: Terminal P2 S35
instance connectS34 :: Connect P2 GameServer S34 S34Connected
instance sendS34 :: Send GameServer S34Connected S36 Init
instance receiveS36Hit :: Receive GameServer S36Hit S37 Hit
instance receiveS36Miss :: Receive GameServer S36Miss S40 Miss
instance receiveS36Loser :: Receive GameServer S36Loser S39 Loser
instance branchS36 :: Branch P2 S36 (Cons "loser" S36Loser (Cons "miss" S36Miss (Cons "hit" S36Hit Nil)))
instance sendS37 :: Send GameServer S37 S38 Attack
instance disconnectS39 :: Disconnect P2 GameServer S39 S35
instance sendS40 :: Send GameServer S40 S41 Attack
instance receiveS38Hit :: Receive GameServer S38Hit S36 Hit
instance receiveS38Miss :: Receive GameServer S38Miss S36 Miss
instance receiveS38Winner :: Receive GameServer S38Winner S39 Winner
instance branchS38 :: Branch P2 S38 (Cons "miss" S38Miss (Cons "winner" S38Winner (Cons "hit" S38Hit Nil)))
instance receiveS41Hit :: Receive GameServer S41Hit S36 Hit
instance receiveS41Miss :: Receive GameServer S41Miss S36 Miss
instance receiveS41Winner :: Receive GameServer S41Winner S39 Winner
instance branchS41 :: Branch P2 S41 (Cons "miss" S41Miss (Cons "winner" S41Winner (Cons "hit" S41Hit Nil)))

foreign import data P1 :: Role

instance roleNameP1 :: RoleName P1 "P1"

foreign import data S14 :: Type
foreign import data S14Connected :: Type
foreign import data S16 :: Type
foreign import data S17 :: Type
foreign import data S17Hit :: Type
foreign import data S17Miss :: Type
foreign import data S17Winner :: Type
foreign import data S19 :: Type
foreign import data S20 :: Type
foreign import data S20Hit :: Type
foreign import data S20Miss :: Type
foreign import data S20Loser :: Type
foreign import data S18 :: Type
foreign import data S18Hit :: Type
foreign import data S18Miss :: Type
foreign import data S18Loser :: Type
foreign import data S15 :: Type

instance initialP1 :: Initial P1 S14
instance terminalP1 :: Terminal P1 S15
instance connectS14 :: Connect P1 GameServer S14 S14Connected
instance sendS14 :: Send GameServer S14Connected S16 Init
instance sendS16 :: Send GameServer S16 S17 Attack
instance receiveS17Hit :: Receive GameServer S17Hit S18 Hit
instance receiveS17Miss :: Receive GameServer S17Miss S20 Miss
instance receiveS17Winner :: Receive GameServer S17Winner S19 Winner
instance branchS17 :: Branch P1 S17 (Cons "winner" S17Winner (Cons "miss" S17Miss (Cons "hit" S17Hit Nil)))
instance disconnectS19 :: Disconnect P1 GameServer S19 S15
instance receiveS20Hit :: Receive GameServer S20Hit S16 Hit
instance receiveS20Miss :: Receive GameServer S20Miss S16 Miss
instance receiveS20Loser :: Receive GameServer S20Loser S19 Loser
instance branchS20 :: Branch P1 S20 (Cons "hit" S20Hit (Cons "loser" S20Loser (Cons "miss" S20Miss Nil)))
instance receiveS18Hit :: Receive GameServer S18Hit S16 Hit
instance receiveS18Miss :: Receive GameServer S18Miss S16 Miss
instance receiveS18Loser :: Receive GameServer S18Loser S19 Loser
instance branchS18 :: Branch P1 S18 (Cons "hit" S18Hit (Cons "loser" S18Loser (Cons "miss" S18Miss Nil)))

foreign import data GameServer :: Role

instance roleNameGameServer :: RoleName GameServer "GameServer"

foreign import data S67 :: Type
foreign import data S69 :: Type
foreign import data S70 :: Type
foreign import data S71 :: Type
foreign import data S71Hit :: Type
foreign import data S71Miss :: Type
foreign import data S71Winner :: Type
foreign import data S86 :: Type
foreign import data S72 :: Type
foreign import data S80 :: Type
foreign import data S73 :: Type
foreign import data S78 :: Type
foreign import data S81 :: Type
foreign import data S74 :: Type
foreign import data S74Hit :: Type
foreign import data S74Miss :: Type
foreign import data S74Winner :: Type
foreign import data S79 :: Type
foreign import data S82 :: Type
foreign import data S82Hit :: Type
foreign import data S82Miss :: Type
foreign import data S82Winner :: Type
foreign import data S83 :: Type
foreign import data S68 :: Type
foreign import data S84 :: Type
foreign import data S85 :: Type
foreign import data S75 :: Type
foreign import data S76 :: Type
foreign import data S77 :: Type

instance initialGameServer :: Initial GameServer S67
instance terminalGameServer :: Terminal GameServer S68
instance acceptS67 :: Accept GameServer P1 S67 S69
instance acceptS69 :: Accept GameServer P2 S69 S70
instance receiveS70 :: Receive P1 S70 S71 Attack
instance sendS71Hit :: Send P1 S71Hit S72 Hit
instance sendS71Miss :: Send P1 S71Miss S80 Miss
instance sendS71Winner :: Send P1 S71Winner S86 Winner
instance selectS71 :: Select P1 S71 (Cons "miss" S71Miss (Cons "hit" S71Hit (Cons "winner" S71Winner Nil)))
instance sendS86 :: Send P2 S86 S78 Loser
instance sendS72 :: Send P2 S72 S73 Hit
instance sendS80 :: Send P2 S80 S81 Miss
instance receiveS73 :: Receive P2 S73 S74 Attack
instance disconnectS78 :: Disconnect GameServer P1 S78 S79
instance receiveS81 :: Receive P2 S81 S82 Attack
instance sendS74Hit :: Send P2 S74Hit S75 Hit
instance sendS74Miss :: Send P2 S74Miss S76 Miss
instance sendS74Winner :: Send P2 S74Winner S77 Winner
instance selectS74 :: Select P2 S74 (Cons "winner" S74Winner (Cons "hit" S74Hit (Cons "miss" S74Miss Nil)))
instance disconnectS79 :: Disconnect GameServer P2 S79 S68
instance sendS82Hit :: Send P2 S82Hit S83 Hit
instance sendS82Miss :: Send P2 S82Miss S84 Miss
instance sendS82Winner :: Send P2 S82Winner S85 Winner
instance selectS82 :: Select P2 S82 (Cons "miss" S82Miss (Cons "winner" S82Winner (Cons "hit" S82Hit Nil)))
instance sendS83 :: Send P1 S83 S70 Hit
instance sendS84 :: Send P1 S84 S70 Miss
instance sendS85 :: Send P1 S85 S78 Loser
instance sendS75 :: Send P1 S75 S70 Hit
instance sendS76 :: Send P1 S76 S70 Miss
instance sendS77 :: Send P1 S77 S78 Loser

