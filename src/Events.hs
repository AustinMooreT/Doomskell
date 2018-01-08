module Events where

import Control.Monad.State.Lazy

data Game = Game

type Handler = State Game

data InputEvent =
  InputEvent
  {
    inputName :: String,
    inputHandler :: Handler
  }

