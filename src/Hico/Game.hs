{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Hico.Game (
    runHicoGame
  , getSDLGameState
  , get
  , set
  , msg
  , clear
  , text
  , image
  , imageSection
  , loadImage
  , scaleImage
  , scaleSprite
  , exit
) where

import           Control.Monad        (forever, void)
import           Control.Monad.IO.Class
import qualified Control.Monad.State    as State hiding (state)
import           Data.StateVar          (($=))
import           Data.Text              (pack)
import           Data.Word
import           Foreign.C.Types        (CFloat, CInt)
import           Foreign.Ptr
import           Foreign.Storable
import           Hico.Types
import           Hico.Internal.InputHandling
import           Prelude                hiding (log)
import qualified SDL                    as SDL 
import qualified SDL.Font
import qualified SDL.Image
import           SDL.Internal.Numbered
import qualified SDL.Internal.Types     (Window(..))
import qualified SDL.Raw
import           SDL.Raw.Video          (getWindowPixelFormat)
import qualified SDL.Video.Renderer
import           System.Exit            (exitSuccess)

runHicoGame :: Game e d -> IO ()
runHicoGame game = do
  SDL.initializeAll
  SDL.Font.initialize
  SDL.Image.initialize [SDL.Image.InitPNG]
  window <- SDL.createWindow "My SDL Application" (windowConfig $ config game)
  renderer <- SDL.createRenderer window (-1) (rendererConfig $ config game)
  -- (SDL.rendererLogicalSize renderer) $= Just (logicalSizeSDL (config game))
  -- (SDL.rendererScale renderer) $= scaleSDL (config game)
  font <- SDL.Font.load defaultFontPath 16
  gameLoop (window, renderer, font) game

gameLoop :: SDLBaseState -> Game e d -> IO ()
gameLoop baseState game @ (Game initial config update draw ioData) = do
  void $ State.runStateT op initialGameState
  where
    (window, renderer, font) = baseState
    initialGameState = (SDLGameState config window renderer font 0 [] initial)
    op = forever $ do
      ddata <- ioData
      event <- SDL.pollEvent
      let action = actionFromEvent event
      
      handleAction action
      
      gameState <- getSDLGameState
      setSDLGameState $ gameState { _frameCount = (_frameCount gameState + 1) }
      -- User stuff
      gameState <- getSDLGameState
      update (_state gameState) (_buttons gameState)
      updatedState <- get
      draw updatedState ddata
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


type SDLBaseState = (SDL.Window, SDL.Renderer, SDL.Font.Font)

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
  texture <- SDL.createTextureFromSurface renderer surface
  (w, h) <- SDL.Font.size font t'
  let rt = makeRect x y w h
  msg $ show rt
  SDL.copy renderer texture Nothing (Just rt)
  SDL.freeSurface surface
  SDL.destroyTexture texture
  where
    t'  = pack s
    c'   = colorToRGB c

image :: Sprite -> HicoProgram state ()
image sprite = do
  let (img, anchor) = sprite
  let Image surf = img
  let SDL.V2 x y = sdlPointToVec anchor
  window   <- _window <$> getSDLGameState
  renderer <- _renderer <$> getSDLGameState
  screen   <- SDL.getWindowSurface window
  SDL.surfaceBlit surf Nothing screen Nothing
  return ()

loadImage :: MonadIO m => FilePath -> m HicoImage
loadImage fpath = liftIO $ Image <$> SDL.Image.load fpath

-- TODO: a good candidate for liquid haskell
imageSection :: HicoImage -> ImageBox -> HicoImage
imageSection (Image surf) box                        =  ImageSeg surf box
imageSection (ImageSeg surf boxOrig) box  =  ImageSeg surf box

spriteSection :: Sprite -> ImageBox -> Sprite
spriteSection (img, anchor) box = (imageSection img box, anchor)

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

makePoint :: Int -> Int -> SDL.Point SDL.V2 CInt
makePoint x y = SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))

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
windowConfig (GameConfig wBase hBase scale _) = SDL.WindowConfig
  { SDL.windowBorder          = True
  , SDL.windowHighDPI         = True
  , SDL.windowInputGrabbed    = False
  , SDL.windowMode            = SDL.Windowed
  , SDL.windowGraphicsContext = SDL.NoGraphicsContext
  , SDL.windowPosition        = SDL.Wherever
  , SDL.windowResizable       = False
  , SDL.windowInitialSize     =
    SDL.V2 (fromIntegral (wBase * scale)) (fromIntegral (hBase * scale))
  , SDL.windowVisible         = True
  }


-- TODO: indead of using the following functions, scale individual surfaces
-- Note: this will require we also upscale all image manipulations accordingly
--   this may already be handled by https://wiki.libsdl.org/SDL_RenderSetScale

{- -- Not used
logicalSizeSDL :: GameConfig -> SDL.V2 CInt
logicalSizeSDL (GameConfig widthBase heightBase _ _ )
  = SDL.V2 (fromIntegral widthBase) (fromIntegral heightBase)
-}

{- -- Not used
scaleSDL :: GameConfig -> SDL.V2 CFloat
scaleSDL (GameConfig _ _ scale _) =
  let floatScale = fromIntegral scale
  in  SDL.V2 floatScale floatScale
-}

scaleSprite :: Sprite -> Int -> HicoProgram state Sprite
scaleSprite sprite scale = do
  imgScaled <- scaleImage img scale
  return (imgScaled, anchorScaled) 
  where
    (img, anchor) = sprite
    anchorScaled = fmap (*scale) anchor

scaleImage :: HicoImage -> Int -> HicoProgram state HicoImage
scaleImage imgIn scale = do
  -- FIXME: working:     <- return (surf, box)
  -- FIXME: NOT working: <- scaleSurface surf box scale
  (surfOut, mBoxOut) <- scaleSurface surf box scale
  return $ case (imgIn, mBoxOut) of
    (ImageSeg _ _, Just boxOut) -> ImageSeg surfOut boxOut
    (ImageSeg _ _, Nothing)     -> Image surfOut
    (Image _, _)                -> Image surfOut
  where
    (surf, box) = case imgIn of ImageSeg sf ibox -> (sf, Just ibox)
                                Image sf         -> (sf, Nothing)

scaleSurface :: SDL.Surface -> Maybe ImageBox -> Int -> 
  HicoProgram state (SDL.Surface, Maybe ImageBox)
scaleSurface surfIn box scale = do
  surfInDims <- SDL.surfaceDimensions surfIn
  boxOut <- return $ getBoxFinal surfInDims
  boxOutScaled <- return $ fmap (*scale) boxOut
  boxOutScaledC <- return $ fmap fromIntegral boxOutScaled
  sizeCIntScaled <- return $ boxSize boxOutScaledC
  surfOut <- createScreenSurface sizeCIntScaled
  _ <- SDL.surfaceBlitScaled surfIn (Just (fmap fromIntegral boxOut)) surfOut (Just boxOutScaledC)
  return (surfOut, return boxOutScaled)
  where
    getBoxFinal :: SDL.V2 CInt -> ImageBox
    getBoxFinal surfDims = case box of
      Just b -> b
      Nothing -> do
        SDL.Rectangle origin (fmap fromIntegral surfDims)
    origin :: SDL.Point SDL.V2 Int
    origin = SDL.P (SDL.V2 0 0)

createScreenSurface :: Integral a => SDL.V2 a -> HicoProgram state SDL.Surface
createScreenSurface size = do
  window   <- _window <$> getSDLGameState
  rawPixFmt <- getWindowPixelFormat $ rawWindow window
  let pixFmt :: SDL.Video.Renderer.PixelFormat = fromNumber rawPixFmt
  SDL.createRGBSurface sizeCInt pixFmt
  where
    sizeCInt = fmap fromIntegral size
    rawWindow :: SDL.Window -> SDL.Raw.Window
    rawWindow win = rw
      where SDL.Internal.Types.Window (rw) = win
    
rendererConfig :: GameConfig -> SDL.RendererConfig
rendererConfig (GameConfig _ _ _ rtype) = SDL.RendererConfig
  {
    SDL.rendererType  = rtype
  , SDL.rendererTargetTexture = False
  }

-- TODO figure out how to get from Raw.PixelFormat to Word32
-- NOTE currently using getWindowPixelFormat in createScreenSurface
{-
surfPixFmt2PixFmt  :: SurfacePixelFormat -> IO SDL.PixelFormat
surfPixFmt2PixFmt sPixFmt = do
  rpf <- peek ptrRPF
  fromNumber rpf
    where
      SurfacePixelFormat ptrRPF = sPixFmt
-} 