{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Hico

data SomeEnv = SomeEnv {
  x :: Int
} deriving (Eq, Show)

config' :: GameConfig
config' = GameConfig {
  width = 320,
  height = 240
}

update' :: SomeEnv -> HicoProgram SomeEnv ()
update' state = do
  config <- getConfig
  set $ state { x = ((x state + 1) `mod` 16) }

draw' :: SomeEnv ->  HicoProgram SomeEnv ()
draw' state = do
  clear Black
  text 60 42 "HELLO WORLD!" (toEnum $ x state)

exampleGame :: Game SomeEnv
exampleGame = Game {
  initial = SomeEnv 0,
  config = config',
  update = update',
  draw = draw'
}

main :: IO ()
main = runHicoGame exampleGame
