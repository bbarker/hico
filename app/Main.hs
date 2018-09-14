{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Data.Semigroup ((<>))
import           Hico
import           Options.Applicative
import           SDL            (RendererConfig, RendererType(SoftwareRenderer), defaultRenderer, rendererType)

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

data RunConfig = RunConfig {
  renderer :: RendererType
}

sdlDefaultRendererP :: Parser RendererType
sdlDefaultRendererP = flag' (rendererType(defaultRenderer)) (
  long "default_rndr"
  <> help "Use SDL's default renderer")

sdlsoftwareRendererP :: Parser RendererType
sdlsoftwareRendererP = flag' SoftwareRenderer (
  long "software_rndr"
  <> help "Use SDL's default renderer")

rendererP :: Parser RendererType
rendererP = sdlDefaultRendererP <|> sdlsoftwareRendererP

parseCliConfig :: Parser RunConfig
parseCliConfig = RunConfig <$> rendererP

main :: IO ()
main = doConfig =<< execParser opts
  where
    opts = info (parseCliConfig <**> helper) (
      fullDesc
      <> progDesc "Welcome to Hico!"
      <> header "hico - a minimal example for the hico library"
      )


doConfig :: RunConfig -> IO()
doConfig (RunConfig runConf) = runHicoGame exampleGame
