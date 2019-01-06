module Game.BattleShips where

import Prelude
import Data.List (List)

data PlayerTile
 = Empty
 | Ship Boolean

data OpponentTile
 = Unknown
 | Hit
 | Miss

newtype Board a = Board (List a)
