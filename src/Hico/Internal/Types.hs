{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hico.Internal.Types where

import           Control.Monad.State
import           Data.Symbol         as Symbol
--import           Foreign.C.Types        (CInt)
import           Prelude             hiding (log)
import           SDL                 as SDL
import           SDL.Font            as SDL

data GameConfig = GameConfig {
  widthBase  :: Int,
  heightBase :: Int,
  width      :: Int,
  height     :: Int,
  renderer   :: RendererType
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
  _window     :: Window,
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
  draw    :: e -> HicoProgram e (),
  sprites :: IO [Sprite]
}

type ImageBox = Rectangle Int
data HicoImage
  = Image Surface
  | ImageSeg Surface ImageBox

type ScreenAnchor = Point V2 Int
type Sprite = (HicoImage, ScreenAnchor)

sdlPointToVec :: Point vx entries -> vx entries
sdlPointToVec pVec = do
  let P vec = pVec
  vec

originAnchor :: ScreenAnchor
originAnchor = SDL.P $ V2 0 0

type HicoSymbol = Symbol.Symbol

symbol :: String -> HicoSymbol
symbol name = Symbol.intern name

symbol2Str :: HicoSymbol -> String
symbol2Str sym = Symbol.unintern sym
