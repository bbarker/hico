{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hico.Internal.Types where

import           Control.Monad.State
import           Prelude             hiding (log)
import           SDL                 as SDL
import           SDL.Font            as SDL

data GameConfig = GameConfig {
  width  :: Int,
  height :: Int
}

type Key = Keycode

data Color
  = Black | DarkBlue | DarkPurple | DarkGreen
  | Brown | DarkGray | LightGray | White
  | Red | Orange | Yellow | Green
  | Blue | Indigo | Pink | Peach
  deriving (Show, Eq, Enum)

data SDLGameState state = SDLGameState {
  _config      :: GameConfig,
  _renderer    :: Renderer,
  _font        :: SDL.Font,
  _frameCount  :: Int,
  _keysPressed :: [Key],
  _state       :: state
}

type HicoProgram state = StateT (SDLGameState state) IO

data Game e = Game {
  initial :: e,
  config  :: GameConfig,
  update  :: e -> HicoProgram e (),
  draw    :: e -> HicoProgram e ()
}
