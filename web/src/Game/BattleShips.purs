module Game.BattleShips
  ( Board
  , PlayerTile(..)
  , OpponentTile(..)
  , Result(..)
  , size
  )
  where

import Prelude
import Data.Array (zipWith)
import Data.Newtype (class Newtype)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (replicate)

data PlayerTile
  = Empty
  | Ship Boolean -- Whether the ship has been hit

instance playerTileMonoid :: Monoid PlayerTile where
  mempty = Empty

instance playerTileSemigroup :: Semigroup PlayerTile where
  append Empty x = x
  append x Empty = x
  append (Ship x) (Ship y) = Ship  $ x || y

instance playerTileShow :: Show PlayerTile where
  show = case _ of
    Empty        -> "~"
    (Ship true)  -> "x"
    (Ship false) -> "s"

data OpponentTile
 = Unknown
 | HitShip
 | Missed

instance opponentTileShow :: Show OpponentTile where
  show = case _ of
    Unknown -> "?"
    HitShip -> "*"
    Missed  -> "."

data Result
 = Hit
 | Miss

newtype Board a = Board (Array a)
derive instance boardNewtype :: Newtype (Board a) _

instance boardSemigroup :: Semigroup a => Semigroup (Board a) where
  append (Board b) (Board b') = Board $ zipWith append b b'
instance boardMonoid :: Monoid a => Monoid (Board a) where
  mempty = Board $ replicate size mempty

-- A board is 5 units accross
size :: Int
size = 5
