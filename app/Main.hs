{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Hico
import           Options.Applicative
import qualified Data.Map.Strict        as DMapS
-- TODO: abstract SDL Image load above

data SomeEnv = SomeEnv {
  _x       :: Int,
  _y       :: Int,
  _color   :: Int,
  _sprites :: [ImageId]
} deriving (Eq, Show)

handleInput :: SomeEnv -> Button -> (Int, Int)
handleInput (SomeEnv x y _ _) button =
  case button of
    BtnUp    -> (x, y - 1)
    BtnDown  -> (x, y + 1)
    BtnLeft  -> (x - 1, y)
    BtnRight -> (x + 1, y)

type SpriteMap = DMapS.Map ImageId Sprite
sprites' :: IO SpriteMap
sprites' = do
  img <- loadImage imagePath
  return $ DMapS.singleton fullImage (img, originAnchor)

update' :: SomeEnv -> [Button] -> HicoProgram SomeEnv ()
update' env buttons = do
  let (newX, newY) = case buttons of
        [button] ->  handleInput env button
        _        -> (_x env, _y env)

  let newColor = (_color env + 1) `mod` 16

  set $ env {
      _color = newColor
    , _x = newX
    , _y = newY
  }

-- TODO: add current sprites to draw to SomeEnv
draw' :: SomeEnv -> SpriteMap -> HicoProgram SomeEnv ()
draw' env spriteMap = do
  clear Black
  mapM image (spriteMap DMapS.!? fullImage)
  text x y "HELLO WORLD!" color
  where
    color = toEnum $ _color env
    x = _x env
    y = _y env


exampleGame :: GameConfig -> Game SomeEnv
exampleGame cfg = Game {
  initial = SomeEnv 0 0 0 [fullImage],
  config  = cfg,
  update  = update',
  draw    = draw',
  sprites  = sprites'
}

main :: IO ()
main = do
  runWithConfig =<< execParser opts
    where
      opts = info (parseCliConfig <**> helper) (
        fullDesc
        <> progDesc "Welcome to Hico!"
        <> header "hico - a minimal example for the hico library"
        )

runWithConfig :: CliConfig -> IO()
runWithConfig runConf =
  runHicoGame (exampleGame (processRunConfig runConf))

imagePath :: FilePath
imagePath = "assets/images/jump_game_160x120.png"
fullImage = imageId "fullImage"