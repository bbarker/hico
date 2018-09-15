{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DuplicateRecordFields #-}


module Main where

import           Data.Semigroup ((<>))
import           Hico
import           Options.Applicative
import           SDL            (RendererConfig, RendererType(SoftwareRenderer), defaultRenderer, rendererType)

data SomeEnv = SomeEnv {
  x :: Int
} deriving (Eq, Show)


update' :: SomeEnv -> HaxelProgramF SomeEnv ()
update' state = do
  config <- getConfig
  setState $ state { x = ((x state + 1) `mod` (width config)) }


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

data RawRunConfig = RawRunConfig {
  widthIn  :: Int,
  heightIn :: Int,
  renderer :: Maybe RendererType
}

processRunConfig :: RawRunConfig -> GameConfig
processRunConfig raw = GameConfig{
   width    = widthIn(raw)
   ,height  = heightIn(raw)
  ,renderer = maybe defaultRendererType id (rendererRaw(raw))
  }
  where
    rendererRaw(raw) = renderer(raw:: RawRunConfig)


xresP :: Parser Int
xresP = option auto(
  long "horizontal-res"
  <> short 'x'
  <> help "Width; Horizontal (x) axis resolution for game"
  <> showDefault
  <> value 640
  <> metavar "INT"
  )

yresP :: Parser Int
yresP = option auto
          ( long "vertical-res"
         <> short 'y'
         <> help "Height; Vertical (y) axis resolution for game"
         <> showDefault
         <> value 480
         <> metavar "INT")

defaultRendererType :: RendererType
defaultRendererType = rendererType(defaultRenderer)

sdlDefaultRendererP :: Parser RendererType
sdlDefaultRendererP = flag' defaultRendererType (
  long "default_rndr"
  <> help "Use SDL's default renderer")

sdlsoftwareRendererP :: Parser RendererType
sdlsoftwareRendererP = flag' SoftwareRenderer (
  long "software_rndr"
  <> help "Use SDL's default renderer")

rendererP :: Parser (Maybe RendererType)
rendererP = optional (sdlDefaultRendererP <|> sdlsoftwareRendererP)

parseCliConfig :: Parser RawRunConfig
parseCliConfig = RawRunConfig <$> xresP <*> yresP <*> rendererP

main :: IO ()
main = doConfig =<< execParser opts
  where
    opts = info (parseCliConfig <**> helper) (
      fullDesc
      <> progDesc "Welcome to Hico!"
      <> header "hico - a minimal example for the hico library"
      )


doConfig :: RawRunConfig -> IO()
doConfig runConf =
  runHicoGame (exampleGame(processRunConfig(runConf)))
