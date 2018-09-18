{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Hico
import           Options.Applicative


data SomeEnv = SomeEnv {
  _x     :: Int,
  _y     :: Int,
  _color :: Int
} deriving (Eq, Show)

handleInput :: SomeEnv -> Button -> (Int, Int)
handleInput (SomeEnv x y _) button =
  case button of
    BtnUp    -> (x, y - 1)
    BtnDown  -> (x, y + 1)
    BtnLeft  -> (x - 1, y)
    BtnRight -> (x + 1, y)

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

draw' :: SomeEnv ->  HicoProgram SomeEnv ()
draw' env = do
  clear Black
  text x y "HELLO WORLD!" color
  where
    color = toEnum $ _color env
    x = _x env
    y = _y env


exampleGame :: GameConfig -> Game SomeEnv
exampleGame cfg = Game {
  initial = SomeEnv 0 0 0,
  config = cfg,
  update = update',
  draw = draw'
}

main :: IO ()
main = doConfig =<< execParser opts
  where
    opts = info (parseCliConfig <**> helper) (
      fullDesc
      <> progDesc "Welcome to Hico!"
      <> header "hico - a minimal example for the hico library"
      )

doConfig :: CliConfig -> IO()
doConfig runConf =
  runHicoGame (exampleGame (processRunConfig runConf))
