module Main where

import Prelude

import Concur.React.Run (runWidgetInDom)
import Concur.React.DOM (h1', text)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Hello sailor!"
  runWidgetInDom "root" $ do
    h1' [text "Hello Sailor"]
