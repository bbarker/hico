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

data Action = KeyInput KeyMotion | Idle | Quit
  deriving Show

data KeyMotion
  = Pressed Button
  | Released Button
  deriving Show

data Button
  = BtnUp
  | BtnDown
  | BtnLeft
  | BtnRight
  | BtnA
  | BtnB
  deriving (Show, Eq)

data Color
  = Black | DarkBlue | DarkPurple | DarkGreen
  | Brown | DarkGray | LightGray | White
  | Red | Orange | Yellow | Green
  | Blue | Indigo | Pink | Peach
  deriving (Show, Eq, Enum)

data SDLGameState state = SDLGameState {
  _config     :: GameConfig,
  _renderer   :: Renderer,
  _font       :: SDL.Font,
  _frameCount :: Int,
  _buttons    :: [Button],
  _state      :: state
}

type HicoProgram state = StateT (SDLGameState state) IO

data Game e = Game {
  initial :: e,
  config  :: GameConfig,
  update  :: e -> [Button] -> HicoProgram e (),
  draw    :: e -> HicoProgram e ()
}
