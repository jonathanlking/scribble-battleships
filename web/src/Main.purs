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
main = runWidgetInDom "root" ({-- fibWidget 9160 <> --} battleShipsGameWidget 9161 <> legend)

battleShipsGameWidget :: forall a. Int -> Widget HTML a
battleShipsGameWidget port = forever $ do
  button [unit <$ onClick] [text "Start game"]
  battleShipsWidgetP1 port

battleShipsWidgetP1 :: Int -> Widget HTML Unit
battleShipsWidgetP1 port = session 
  (Proxy :: Proxy WebSocket)
  (Role :: Role BS.P1) $ do
    connect (Role :: Role BS.GameServer) (URL $ "ws://localhost:" <> show port)
    config <- lift setupGameWidget
    send $ BS.Init config
    let pb = BS.mkBoard config
    let ob = mempty :: BS.Board BS.OpponentTile
    attack pb ob
  where
    statusUpdate 
      :: String
      -> BS.Board BS.PlayerTile
      -> BS.Board BS.OpponentTile
      -> Widget HTML Unit
    statusUpdate message pb ob
      = (h4' [text message]) <> (button [unit <$ onClick] [text "I'm ready to be attacked!"]) <> (playerBoard pb) <> (opponentBoardWidget ob)

    attack 
      :: BS.Board BS.PlayerTile
      -> BS.Board BS.OpponentTile
      -> Session (Widget HTML) WebSocket BS.S16 BS.S15 Unit
    attack pb ob = do
      loc <- lift $ h4' [text "Choose a location to attack!"] <> playerBoard pb <> moveSelectionWidget ob
      send $ BS.Attack loc
      choice
        { winner: do
            void receive
            disconnect (Role :: Role BS.GameServer)
            lift $ (h4' [text "You won!"]) <> (button [unit <$ onClick] [text "Quit"])
        , miss: do
            BS.Miss loc <- receive
            let ob' = BS.setLocation loc BS.Missed ob
            lift $ statusUpdate "You missed!" pb ob'
            defendAfterMiss pb ob'
        , hit: do
            BS.Hit loc <- receive
            let ob' = BS.setLocation loc BS.HitShip ob
            lift $ statusUpdate "You hit!" pb ob'
            defendAfterHit pb ob'
        } -- `whileWaiting` (playerBoard pb <> h4' [(text "Waiting for response")])

    -- TODO: Investigate why the state space split
    defendAfterMiss
      :: BS.Board BS.PlayerTile
      -> BS.Board BS.OpponentTile
      -> Session (Widget HTML) WebSocket BS.S20 BS.S15 Unit
    defendAfterMiss pb ob =
      choice
        { loser: do
            void receive
            disconnect (Role :: Role BS.GameServer)
            lift $ (h4' [text "You lost!"]) <> (button [unit <$ onClick] [text "Quit"])
        , hit: do
            BS.Hit loc <- receive
            let pb' = BS.setLocation loc (BS.Ship true) pb
            attack pb' ob
        , miss: do 
            BS.Miss loc <- receive
            let pb' = BS.setLocation loc (BS.MissedPT) pb
            attack pb' ob
        }
    defendAfterHit
      :: BS.Board BS.PlayerTile
      -> BS.Board BS.OpponentTile
      -> Session (Widget HTML) WebSocket BS.S18 BS.S15 Unit
    defendAfterHit pb ob =
      choice
        { loser: do
            void receive
            disconnect (Role :: Role BS.GameServer)
            lift $ h4' [text "You lost!"]
        , hit: do
            BS.Hit loc <- receive
            let pb' = BS.setLocation loc (BS.Ship true) pb
            attack pb' ob
        , miss: do 
            BS.Miss loc <- receive
            attack pb ob
        }

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
moveSelectionWidget :: BS.Board BS.OpponentTile -> Widget HTML BS.Location
moveSelectionWidget b
  = let mkTile i t = button [disabled (not $ BS.playable t), BS.Location i <$ onClick] [text $ show t]
    in h4' [text "Opponent's board:"] <|> (fold $ iover itraversed mkTile $ unwrap b)

opponentBoardWidget :: forall a. BS.Board BS.OpponentTile -> Widget HTML a
opponentBoardWidget b
  = let mkTile i t = button [disabled true] [text $ show t]
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
