module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, div', h4', text, input, p')
import Concur.React.DOM as D
import Concur.React.Props (onClick, _type, value, onChange, checked, disabled)
import Concur.React.Props as P
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (negateDuration)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (now)
import Control.Monad.Rec.Class (forever)
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (isJust, maybe)
import Data.Int (fromString)
import Unsafe.Coerce (unsafeCoerce)
import Type.Proxy (Proxy(..))
import Data.Symbol (SProxy(..))

import Scribble.Protocol.Arithmetic.MathServer as MS
import Scribble.Protocol.Game.BattleShips as BS

import Scribble.FSM (Role(..))
import Scribble.Session (Session, session, connect, send, receive, lift, whileWaiting, select, choice, disconnect)
import Scribble.Transport.WebSocket (WebSocket, URL(..))
import Control.Bind.Indexed (ibind)
import Control.Applicative.Indexed (ipure)

import Data.Newtype (wrap, unwrap)
import Data.Foldable (fold)
import Game.BattleShips as BS
import Data.Lens.Indexed (itraversed)
import Data.Lens.Setter (iover)

main :: Effect Unit
-- main = runWidgetInDom "root" (fibWidget 9160)
main = runWidgetInDom "root" (fibWidget 9160 <> fibWidget 9160 <> legend)

-- gameWidget :: forall a. Widget HTML a
-- gameWidget = do
--   config <- setupGameWidget
--   -- Send our config to the Board role
--   let board = BS.mkBoard config
--   x <- playerBoard board <|> opponentBoard mempty
--   liftEffect $ log $ show x
--   playerBoard board

battleShipsWidgetP1 :: forall a. Int -> Widget HTML a
battleShipsWidgetP1 port = session 
  (Proxy :: Proxy WebSocket)
  (Role :: Role BS.P1) $ do
    connect (Role :: Role BS.GameServer) (URL $ "ws://localhost:" <> show port)
    config <- lift setupGameWidget
    send $ BS.Init config
    let pb = BS.mkBoard config
    let ob = mempty :: BS.Board BS.OpponentTile
    loc <- lift $ playerBoard pb <|> opponentBoard ob
    attack pb ob
  where
    attack 
      :: forall a. 
         BS.Board BS.PlayerTile
      -> BS.Board BS.OpponentTile
      -> Session (Widget HTML) WebSocket BS.S16 BS.S15 a
    attack pb ob = do
      loc <- lift $ playerBoard pb <|> opponentBoard ob
      send $ BS.Attack loc
      choice
       { winner: do
           void receive
           disconnect (Role :: Role BS.GameServer)
           lift $ text "You won!"
       , miss: do
           BS.Miss loc <- receive `whileWaiting` (text "please... work! :(")
           let ob' = BS.setLocation loc BS.Missed ob
           defendAfterMiss pb ob'
       , hit: do
           BS.Hit loc <- receive
           let ob' = BS.setLocation loc BS.HitShip ob
           defendAfterHit pb ob'
       }

    -- TODO: Investigate why the state space split
    defendAfterMiss
      :: forall a.
         BS.Board BS.PlayerTile
      -> BS.Board BS.OpponentTile
      -> Session (Widget HTML) WebSocket BS.S20 BS.S15 a
    defendAfterMiss pb ob = unsafeCoerce unit

    defendAfterHit
      :: forall a.
         BS.Board BS.PlayerTile
      -> BS.Board BS.OpponentTile
      -> Session (Widget HTML) WebSocket BS.S18 BS.S15 a
    defendAfterHit _ _ = unsafeCoerce unit

    bind = ibind
    pure = ipure
    discard = bind

-- Produces the board config for the player
setupGameWidget :: Widget HTML BS.Config
setupGameWidget = do
  let mkTile i t = button [BS.mkConfig i <$ onClick] [text $ show t]
      board = unwrap $ mempty :: BS.Board BS.PlayerTile
  h4' [text "Place your ship:"] <|> (fold $ iover itraversed mkTile board)

playerTileWidget :: forall a. BS.PlayerTile -> Widget HTML a
playerTileWidget t 
  = button [disabled true] [text $ show t]

playerBoard :: forall a. BS.Board BS.PlayerTile -> Widget HTML a
playerBoard b
  = h4' [text "Your board:"] <|> (fold $ playerTileWidget <$> unwrap b)

-- The board will produce the index of a valid move played
opponentBoard :: BS.Board BS.OpponentTile -> Widget HTML BS.Location
opponentBoard b
  = let mkTile i t = button [disabled (not $ BS.playable t), BS.Location i <$ onClick] [text $ show t]
    in h4' [text "Opponent's board:"] <|> (fold $ iover itraversed mkTile $ unwrap b)

legend :: forall a. Widget HTML a
legend = div'
    [ h4'[text "Legend"]
    , p' [text "~ = Empty sea"]
    , p' [text "s = Your ship"]
    , p' [text "? = Unknown enemy sea"]
    , p' [text "* = Hit enemy ship"]
    , p' [text "! = Missed attack"]
    ]

-- MathServer fibonacci example

fib :: forall v. Monoid v => Int -> Session (Widget v) WebSocket MS.S13 MS.S13 Int
fib n
  | n <= 1    = pure 1
  | otherwise = do
     x <- fib (n - 1)
     y <- fib (n - 2)
     select (SProxy :: SProxy "add")
     send (MS.Add x y)
     MS.Sum s <- receive
     pure s
  where
    bind = ibind
    pure = ipure
    discard = bind

fibWidget :: forall a. Int -> Widget HTML a
fibWidget port = do
  _ <- button [onClick] [text "Connect to MathServer"]
  res <- sessionFibWidget port
  _ <- h4' [text $ show res] <> button [onClick] [text "Reset"]
  fibWidget port
  
sessionFibWidget :: forall a. Int -> Widget HTML Int
sessionFibWidget port = session 
  (Proxy :: Proxy WebSocket)
  (Role :: Role MS.Client) $ do
  connect (Role :: Role MS.Server) (URL $ "ws://localhost:" <> show port)
  send MS.Connect
  x <- lift $ fibFormWidget Nothing
  res <- (fib x) `whileWaiting` (text "Calculating...")
  select (SProxy :: SProxy "quit")
  send MS.Quit
  disconnect (Role :: Role MS.Server)
  pure res
  where
    bind = ibind
    pure = ipure
    discard = bind

data FibFormAction
  = Input String
  | Calculate

fibFormWidget :: Maybe Int -> Widget HTML Int
fibFormWidget input = do
  res <- div'
    [ button [Calculate <$ onClick, value "Submit", disabled (not $ isJust input)] [text "Submit"]
    , Input <$> textInputEnter (maybe "" show input) "e.g. 10" false
    ]
  case res of
    Input s ->
      case fromString s of
        Nothing -> fibFormWidget input
        Just i  -> fibFormWidget (Just i)
    Calculate ->
      case input of
        Nothing -> fibFormWidget Nothing
        Just i  -> pure i

-- | A Text input that returns its contents on enter
textInputEnter :: String -> String -> Boolean -> Widget HTML String
textInputEnter value hint reset = do
    e <- D.input [P.defaultValue value, P.onChange, P.placeholder hint]
    -- HACK: Using forced do notation, to force evaluation of the text input value in the same handler
    new <- pure $ unsafeGetVal e
--    when reset $ liftEffect (P.resetTargetValue "" e)
    pure new
  where
    unsafeGetVal e = (unsafeCoerce e).target.value

-- Counter widget from Concur examples
counterWidget :: forall a. Int -> Widget HTML a
counterWidget count = do
  n <- div'
        [ p' [text ("State: " <> show count)]
        , button [onClick] [text "Increment"] $> count+1
        , button [onClick] [text "Decrement"] $> count-1
        ]
  liftEffect (log ("COUNT IS NOW: " <> show n))
  counterWidget n
