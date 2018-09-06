{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Hico

data SomeEnv = SomeEnv {
  x :: Int
} deriving (Eq, Show)

config' :: GameConfig
config' = GameConfig {
  width = 640,
  height = 480
}

update' :: SomeEnv -> HaxelProgramF SomeEnv ()
update' state = do
  config <- getConfig
  setState $ state { x = ((x state + 1) `mod` (width config)) }


draw' :: SomeEnv ->  HaxelProgramF SomeEnv ()
draw' state = do
  clear Red
  rect 10 10 20 20 Green

exampleGame :: Game SomeEnv
exampleGame = Game {
  initial = SomeEnv 1,
  config = config',
  update = update',
  draw = draw'
}

main :: IO ()
main = runHicoGame exampleGame
