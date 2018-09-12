{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Data.Semigroup ((<>))
import           Hico
import           Options.Applicative
import           SDL            (RendererConfig, RendererType(SoftwareRenderer), defaultRenderer)

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

defaultRendererType :: RendererType
defaultRendererType = case defaultRenderer of
  RendererConfig rt  _ -> rt


sdlDefaultRenderer :: Parser RendererType
sdlDefaultRenderer = flag' defaultRendererType (
  long "renderer=default"
  <> help "Use SDL's default renderer")

sdlsoftwareRenderer :: Parser RendererType
sdlsoftwareRenderer = flag' SoftwareRenderer (
  long "renderer=default"
  <> help "Use SDL's default renderer")

parseCliConfig :: Parser RunConfig
parseCliConfig = RunConfig
  <$> option auto (
    long "renderer"
    <> metavar "RENDERER"
    <> help "Which SDL RenderType to use"
  )

main :: IO ()
main = doConfig =<< execParser opts
  where
    opts = info (parseCliConfig <**> helper) (
      fullDesc
      <> progDesc "Welcome to Hico!"
      <> header "hico - a minimal example for the hico library"
      )


doConfig :: RunConfig -> IO()
doConfig (RunConfig renderer) = runHicoGame exampleGame
doConfig _ = return ()