{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DuplicateRecordFields #-}

module Hico.Config.Cli where

import           Data.Semigroup ((<>))
import           Data.Maybe
import           Hico.Types
import           Options.Applicative
import           SDL (RendererType(SoftwareRenderer), defaultRenderer, rendererType)

data CliConfig = CliConfig {
  widthIn  :: Int,
  heightIn :: Int,
  renderer :: Maybe RendererType
}

processRunConfig :: CliConfig -> GameConfig
processRunConfig raw = GameConfig{
   width    = widthIn raw
   ,height  = heightIn raw
  ,renderer = fromMaybe defaultRendererType (rendererRaw raw)
  }
  where
    rendererRaw raw = renderer(raw:: CliConfig)

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
defaultRendererType = rendererType defaultRenderer

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

parseCliConfig :: Parser CliConfig
parseCliConfig = CliConfig <$> xresP <*> yresP <*> rendererP
