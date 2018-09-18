{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Hico

data SomeEnv = SomeEnv {
  _x     :: Int,
  _y     :: Int,
  _color :: Int
} deriving (Eq, Show)

config' :: GameConfig
config' = GameConfig {
  width = 320,
  height = 240
}

clip :: (Ord a) => a -> a -> a -> a
clip n a b = min b $ max n a

handleInput :: (Int, Int) -> Button -> (Int, Int)
handleInput (x, y) button = case button of
  BtnUp    -> (x, clip (y - 1) 0 _height)
  BtnDown  -> (x, clip (y + 1) 0 _height)
  BtnLeft  -> (clip (x - 1) 0 _width, y)
  BtnRight -> (clip (x + 1) 0 _width, y)
  where
    _width = width config'
    _height = height config'

update' :: SomeEnv -> [Button] -> HicoProgram SomeEnv ()
update' env buttons = do
  msg $ show env

  msg $ show buttons
  let (newX, newY) = case buttons of
        [button] ->  handleInput (_x env, _y env) button
        _        -> (_x env, _y env)

  let newColor = (_color env + 1) `mod` 16

  set $ env {
      _color = newColor
    , _x = newX
    , _y = newY
  }

draw' :: SomeEnv ->  HicoProgram SomeEnv ()
draw' env = do
  clear Black
  text x y "HELLO WORLD!" color
  where
    color = toEnum $ _color env
    x = _x env
    y = _y env

exampleGame :: Game SomeEnv
exampleGame = Game {
  initial = SomeEnv 0 0 0,
  config = config',
  update = update',
  draw = draw'
}

main :: IO ()
main = runHicoGame exampleGame
