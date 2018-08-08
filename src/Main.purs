module Main where

import Prelude

import Concur.React.DOM (button, h1', text)
import Concur.React.Props (onClick)
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Hello sailor!"
  runWidgetInDom "root" $ do
    _ <- button [onClick] [text "Hello Sailor!"]
    h1' [text "Hello Sailor!"]
