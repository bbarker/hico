{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Hico
import           Options.Applicative


data SomeEnv = SomeEnv {
  x :: Int
} deriving (Eq, Show)


update' :: SomeEnv -> HaxelProgramF SomeEnv ()
update' state = do
  config <- getConfig
  setState $ state { x = (x state + 1) `mod` (width config) }


draw' :: SomeEnv ->  HaxelProgramF SomeEnv ()
draw' state = do
  clear Red
  rect 10 10 20 20 Green

exampleGame :: GameConfig -> Game SomeEnv
exampleGame cfg = Game {
  initial = SomeEnv 1,
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
