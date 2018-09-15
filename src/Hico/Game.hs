{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hico.Game (
    runHicoGame
) where

import           Control.Monad        (forever, void)
import           Hico.SDL.Interpreter (runHicoSDL)
import           Hico.Types
import           SDL                  as SDL

runHicoGame :: Game e -> IO ()
runHicoGame game = do
  SDL.initializeAll
  window <- SDL.createWindow "My SDL Application" (windowConfig $ config game)
  renderer <- SDL.createRenderer window (-1) (rendererConfig $ config game)
  gameLoop renderer game

gameLoop :: Renderer -> Game e -> IO ()
gameLoop renderer game @ (Game initial config update draw) = do
  events <- SDL.pollEvents
  void $ runHicoSDL renderer config initial (forever op)
  where
    op = do
      state <- getState
      -- update state
      -- updatedState <- getState
      draw state

windowConfig :: GameConfig -> WindowConfig
windowConfig (GameConfig width height _) = WindowConfig
  { windowBorder       = True
  , windowHighDPI      = False
  , windowInputGrabbed = False
  , windowMode         = Windowed
  , windowOpenGL       = Nothing
  , windowPosition     = Wherever
  , windowResizable    = False
  , windowInitialSize  = V2 (fromIntegral width) (fromIntegral height)
  , windowVisible      = True
  }

rendererConfig :: GameConfig -> RendererConfig
rendererConfig (GameConfig _ _ rtype) = RendererConfig
  {
    rendererType  = rtype
  , rendererTargetTexture = False
  }
