{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hico.Game (
    runHicoGame
  , getSDLGameState
  , get
  , set
  , msg
  , clear
  , text
  , exit
) where

import           Control.Monad        (forever, void)
import           Control.Monad.IO.Class
import qualified Control.Monad.State    as State hiding (state)
import           Data.Text              (pack)
import           Data.Word
import           Foreign.C.Types        (CInt)
import           Hico.Types
import           Hico.Internal.InputHandling
import           Prelude                hiding (log)
import qualified SDL                    as SDL 
import qualified SDL.Font 
import           System.Exit            (exitSuccess)

runHicoGame :: Game e -> IO ()
runHicoGame game = do
  SDL.initializeAll
  SDL.Font.initialize
  window <- SDL.createWindow "My SDL Application" (windowConfig $ config game)
  renderer <- SDL.createRenderer window (-1) (rendererConfig $ config game)
  font <- SDL.Font.load defaultFontPath 16
  gameLoop renderer font game

screenHeight = 480
screenWidth = 640

gameLoop :: SDL.Renderer -> SDL.Font.Font -> Game e -> IO ()
gameLoop renderer font game @ (Game initial config update draw) = 
  void $ State.runStateT op initialGameState 
  where
    initialGameState = (SDLGameState config renderer font 0 [] initial)
    op = forever $ do
      event <- SDL.pollEvent
      let action = actionFromEvent event
      
      handleAction action
      
      gameState <- getSDLGameState
      setSDLGameState $ gameState { _frameCount = (_frameCount gameState + 1) }
      
      -- User stuff
      gameState <- getSDLGameState
      update (_state gameState) (_buttons gameState)
      updatedState <- get
      draw updatedState

      SDL.present renderer

handleAction :: Action -> HicoProgram state ()
handleAction action = do
  gameState <- getSDLGameState
  case action of
    KeyInput km -> do
      msg $ show km
      setSDLGameState $ gameState { _buttons = (updateButtons km (_buttons gameState)) }
    Idle        -> pure ()
    Quit        -> exit
  
getSDLGameState :: HicoProgram state (SDLGameState state)
getSDLGameState = State.get

setSDLGameState :: SDLGameState state -> HicoProgram state ()
setSDLGameState = State.put

get :: HicoProgram state state
get =  _state <$> getSDLGameState

set :: state -> HicoProgram state ()
set s = do
  gameState <- getSDLGameState
  setSDLGameState $ gameState { _state = s }

msg :: String -> HicoProgram state ()
msg = liftIO . putStrLn

clear :: Color -> HicoProgram state ()
clear color = do
  renderer <- _renderer <$> getSDLGameState
  sdlSetColor renderer color
  sdlClear renderer

text :: Int -> Int -> String -> Color -> HicoProgram state ()
text x y s c = do
  renderer <- _renderer <$> getSDLGameState
  font <- _font <$> getSDLGameState
  surface <- SDL.Font.solid font c' t'
  texture   <- SDL.createTextureFromSurface renderer surface
  (w, h) <- SDL.Font.size font t'
  let rt = makeRect x y w h 
  msg $ show rt
  SDL.copy renderer texture Nothing (Just rt)
  SDL.freeSurface surface
  SDL.destroyTexture texture
  where
    t'  = pack s
    c'   = colorToRGB c

exit :: HicoProgram state ()
exit = msg "Exiting Hico" >> liftIO exitSuccess

sdlRect :: MonadIO m => SDL.Renderer -> Int -> Int -> Int -> Int -> Color-> m ()
sdlRect renderer x1 y1 x2 y2 color = SDL.drawRect renderer (Just rect) where
  rect = SDL.Rectangle (SDL.P $ SDL.V2 0 0) (SDL.V2 2 4)

sdlSetColor :: MonadIO m => SDL.Renderer -> Color -> m ()
sdlSetColor renderer color = liftIO $ SDL.rendererDrawColor renderer SDL.$= colorToRGB color

sdlClear :: MonadIO m => SDL.Renderer -> m ()
sdlClear renderer = liftIO $ SDL.clear renderer

makeRect :: Int -> Int -> Int -> Int -> SDL.Rectangle CInt
makeRect x y w h = SDL.Rectangle o z
  where
    (x', y', w', h') =
        (fromIntegral x, fromIntegral y, fromIntegral w, fromIntegral h)
    o = SDL.P (SDL.V2 x' y')
    z = SDL.V2 w' h'

defaultFontPath :: FilePath
defaultFontPath = "assets/fonts/PICO-8.ttf"

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
    

windowConfig :: GameConfig -> SDL.WindowConfig
windowConfig (GameConfig _ _ width height _) = SDL.WindowConfig
  { SDL.windowBorder       = True
  , SDL.windowHighDPI      = True
  , SDL.windowInputGrabbed = False
  , SDL.windowMode         = SDL.Windowed
  , SDL.windowOpenGL       = Nothing
  , SDL.windowPosition     = SDL.Wherever
  , SDL.windowResizable    = False
  , SDL.windowInitialSize  = SDL.V2 (fromIntegral width) (fromIntegral height)
  , SDL.windowVisible      = True
  }

rendererConfig :: GameConfig -> SDL.RendererConfig
rendererConfig (GameConfig _ _ _ _ rtype) = SDL.RendererConfig
  {
    SDL.rendererType  = rtype
  , SDL.rendererTargetTexture = False
  }
