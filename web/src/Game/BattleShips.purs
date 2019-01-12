module Game.BattleShips
  ( Board
  , Location(..)
  , Config(..)
  , PlayerTile(..)
  , OpponentTile(..)
  , Result(..)
  , mkConfig
  , playable
  , placeShip
  , mkBoard
  , size
  )
  where

import Prelude
import Data.Array (zipWith)
import Data.Lens (set)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Unfoldable (replicate)

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic) -- From purescript-generics-rep
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)

-- A initial game configuration
newtype Config = Config Int

derive instance configGeneric :: Generic Config _
instance configEncodeJson :: EncodeJson Config where
  encodeJson = genericEncodeJson
instance configDecodeJson :: DecodeJson Config where
  decodeJson = genericDecodeJson

newtype Location = Location Int

derive instance locationGeneric :: Generic Location _
instance locationEncodeJson :: EncodeJson Location where
  encodeJson = genericEncodeJson
instance locationDecodeJson :: DecodeJson Location where
  decodeJson = genericDecodeJson

mkConfig :: Int -> Config
mkConfig = Config

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

derive instance opponentTileEq :: Eq OpponentTile

instance opponentTileSemigroup :: Semigroup OpponentTile where
  append Unknown x = Unknown
  append x Unknown = Unknown
  append Missed Missed = Missed
  append HitShip x = HitShip
  append x HitShip = HitShip

instance opponentTileMonoid :: Monoid OpponentTile where
  mempty = Unknown

instance opponentTileShow :: Show OpponentTile where
  show = case _ of
    Unknown -> "?"
    HitShip -> "*"
    Missed  -> "."

-- Pre: The index is within the board
placeShip :: Int -> Board PlayerTile -> Board PlayerTile
placeShip pos = wrap <<< set (ix pos) (Ship false) <<< unwrap

mkBoard :: Config -> Board PlayerTile
mkBoard (Config pos) = placeShip pos mempty

playable :: OpponentTile -> Boolean
playable = (==) Unknown

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
