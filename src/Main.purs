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

import Scribble.FSM
import Scribble.Session
import Scribble.WebSocket (WebSocket, URL(..))
import Control.Bind.Indexed (ibind)
import Control.Applicative.Indexed (ipure)

pingPong :: forall a. Widget HTML a
pingPong = do
  forever $ do
    button [unit <$onClick] [text "Ping"]
    liftAff (delay (Milliseconds 3000.0)) <|> button [unit <$ onClick] [text "Pong"] <|> text "Waiting"

main :: Effect Unit
main = do
  log "Hello sailor!"
--  runWidgetInDom "root" (pingPong <> formWidget {name: "", rememberMe: false})
  runWidgetInDom "root" (example 1 <> example 2)
--  runWidgetInDom "root" (counterWidget 0 <> loopWidget 9160 <> counterWidget 0 <> loopWidget 9160)
--  runWidgetInDom "root" (sessionFibWidget 9160 <> sessionFibWidget 9161)
--  runWidgetInDom "root" (sessionFibWidget <> sessionFibWidget2)

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

type Form =
  { name :: String
  , rememberMe :: Boolean
  }

fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib' :: forall v. Monoid v => Int -> Session (Widget v) WebSocket MS.S9 MS.S9 Int
fib' n
  | n <= 1    = pure 1
  | otherwise = do
     x <- fib' (n - 1)
     y <- fib' (n - 2)
     select (SProxy :: SProxy "add")
     send (MS.Add x y)
     MS.Sum s <- receive
     pure s
  where
    bind = ibind
    pure = ipure
    discard = bind

-- This is like Elm's Action
data FormAction
  = Name String
  | RememberMe Boolean
  | Submit

fibWidget :: forall a. Widget HTML a
fibWidget = do
  x <- fibFormWidget Nothing
  text (show $ fib x)

sessionFibWidget :: forall a. Int -> Widget HTML Int
sessionFibWidget port = session 
  (Proxy :: Proxy WebSocket)
  (Role :: Role MS.Client) $ do
  connect (Role :: Role MS.Server) (URL $ "ws://localhost:" <> show port)
  x <- lift $ fibFormWidget Nothing
  res <- fib' x
  select (SProxy :: SProxy "quit")
  send MS.Quit
  disconnect (Role :: Role MS.Server)
  pure res
  where
    bind = ibind
    pure = ipure
    discard = bind

example :: forall a. Int -> Widget HTML a
example x = do
  liftEffect (log $ "example" <> show x)
  _ <- button [onClick] [text $ "example" <> show x]
  example x

loopWidget :: forall a. Int -> Widget HTML a
loopWidget port = do
--  liftEffect (log $ "helo" <> show port)
  _ <- button [onClick] [text "start!"]
  res <- sessionFibWidget port
  _ <- (text $ show res) <> button [onClick] [text "Reset"]
  loopWidget port
  

-- sessionFibWidget2 :: forall a. Widget HTML a
-- sessionFibWidget2 = session 
--   (Proxy :: Proxy WebSocket)
--   (Role :: Role MS.Client)
--   (URL $ "ws://localhost:9160") $ do
--   select (SProxy :: SProxy "quit")
--   send MS.Quit
--   lift $ text $ show 5
--   where
--     bind = ibind
--     pure = ipure
--     discard = bind

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

formWidget :: Form -> Widget HTML Form
formWidget form = do
  -- This is like Elm's view function
  res <- div'
--    [ Name <$> input [_type "text", value form.name, onChange]
    [ checkbox form.rememberMe
    , button [Submit <$ onClick, value "Submit"] [text "Submit"]
    , Name <$> textInputEnter form.name form.name false
    ]
  -- This is like Elm's update function
  case res of
    Name s -> formWidget (form {name = s})
    RememberMe b -> formWidget (form {rememberMe = b})
    Submit -> formWidget form
    --Submit -> pure form
  where
    checkbox b = (RememberMe $ not b) <$ input [_type "checkbox", checked b, onChange]
--    textbox v =  Name <$> input [_type "text", value v, onChange]

counterWidget :: forall a. Int -> Widget HTML a
counterWidget count = do
  n <- div'
        [ p' [text ("State: " <> show count)]
        , button [onClick] [text "Increment"] $> count+1
        , button [onClick] [text "Decrement"] $> count-1
        ]
  liftEffect (log ("COUNT IS NOW: " <> show n))
  counterWidget n
