{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hico.Internal.Types where

import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Prelude                hiding (log)
import           SDL            (RendererType)


data GameConfig = GameConfig {
  widthBase  :: Int,
  heightBase :: Int,
  width      :: Int,
  height     :: Int,
  renderer   :: RendererType
}

data Color
  = Black | DarkBlue | DarkPurple | DarkGreen
  | Brown | DarkGray | LightGray | White
  | Red | Orange | Yellow | Green
  | Blue | Indigo | Pink | Peach
  deriving (Show, Eq)

data HicoOp state next
  = GetState (state -> next)
  | SetState state next
  | GetConfig (GameConfig -> next)
  | Clear Color next
  | Rect Int Int Int Int Color next
  | DisplayMessage String next
  | Exit next
  deriving (Functor)

makeFree ''HicoOp

type HaxelProgram state = HicoOp state
type HaxelProgramF state = Free (HaxelProgram state)
type HaxelProgramFIO state = StateT state IO

data Game e = Game {
  initial :: e,
  config  :: GameConfig,
  update  :: e -> HaxelProgramF e (),
  draw    :: e -> HaxelProgramF e ()
}
