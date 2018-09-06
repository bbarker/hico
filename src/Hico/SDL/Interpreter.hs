module Hico.SDL.Interpreter where

import           Control.Monad.Free
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Word
import           Hico.Types
import           Prelude                hiding (log)
import           SDL                    as SDL hiding (get)
import           System.Exit            (exitSuccess)

runHicoSDL :: SDL.Renderer
  -> GameConfig
  -> state
  -> Free (HicoOp state) a
  -> IO (a, state)
runHicoSDL renderer config initialState prg = runStateT (iterM interpret prg) initialState where
  interpret = interpreter renderer config

interpreter :: SDL.Renderer
  -> GameConfig
  -> HaxelProgram state (HaxelProgramFIO state next)
  -> HaxelProgramFIO state next
interpreter renderer config op = before >> action
  where
    before = do
      liftIO $ SDL.pollEvents

    action = case op of
      (DisplayMessage msg next) -> do
        liftIO $ putStrLn msg
        next
      (GetState next) -> do
        state <- get
        next state
      (SetState state next) -> do
        put state
        next
      (GetConfig next) ->
        next config
      (Clear color next) -> do
        -- sdlSetColor renderer color
        liftIO $ SDL.present renderer
        -- sdlClear renderer
        next
      (Rect x1 y1 x2 y2 color next) -> do
        sdlSetColor renderer Black
        sdlClear renderer
        sdlSetColor renderer color
        sdlRect renderer 10 10 10 10 color
        liftIO $ SDL.present renderer
        next
      (Exit next) -> liftIO $ exitSuccess

sdlRect :: MonadIO m => Renderer -> Int -> Int -> Int -> Int -> Color-> m ()
sdlRect renderer x1 y1 x2 y2 color = SDL.drawRect renderer (Just rect) where
  rect = SDL.Rectangle (P $ V2 0 0) (V2 2 4)

sdlSetColor :: MonadIO m => Renderer -> Color -> m ()
sdlSetColor renderer color = liftIO $ SDL.rendererDrawColor renderer $= colorToRGB color

sdlClear :: MonadIO m => Renderer -> m ()
sdlClear renderer = liftIO $ SDL.clear renderer

colorToRGB :: Color -> SDL.V4 Word8
colorToRGB color = case color of
  Black      -> SDL.V4 0 0 0 maxBound
  DarkBlue   -> SDL.V4 29 43 83 maxBound
  DarkPurple -> SDL.V4 126 37 83 maxBound
  DarkGreen  -> SDL.V4 0 135 81 maxBound
  Brown      -> SDL.V4 171 82 54 maxBound
  DarkGray   -> SDL.V4 95 87 79 maxBound
  LightGray  -> SDL.V4 194 195 199 maxBound
  White      -> SDL.V4 maxBound 241 232 maxBound
  Red        -> SDL.V4 maxBound 0 77 maxBound
  Orange     -> SDL.V4 maxBound 163 0 maxBound
  Yellow     -> SDL.V4 maxBound 236 39 maxBound
  Green      -> SDL.V4 0 228 54 maxBound
  Blue       -> SDL.V4 41 173 maxBound maxBound
  Indigo     -> SDL.V4 131 118 156 maxBound
  Pink       -> SDL.V4 maxBound 119 168 maxBound
  Peach      -> SDL.V4 maxBound 204 170 maxBound
