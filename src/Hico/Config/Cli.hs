{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hico.Config.Cli where

import           Data.Semigroup ((<>))
import           Data.Maybe
import           Hico.Types
import           Options.Applicative
import           SDL (RendererType (SoftwareRenderer), defaultRenderer, rendererType)

data CliConfig = CliConfig {
  windowScale :: Float,
  renderer    :: Maybe RendererType
}

widthBaseCfg  = 640
heightBaseCfg = 480

processRunConfig :: CliConfig -> GameConfig
processRunConfig (CliConfig wS _) | wS < 1  = error "Window Scale must be >= 1"
processRunConfig raw = GameConfig {
   widthBase  = widthBaseCfg
  ,heightBase = heightBaseCfg
  ,width      = floor (fromIntegral widthBaseCfg  * windowScale raw)
  ,height     = floor (fromIntegral heightBaseCfg * windowScale raw)
  ,renderer = fromMaybe defaultRendererType (rendererRaw raw)
} where rendererRaw raw = renderer (raw:: CliConfig)

windowScaleP :: Parser Float
windowScaleP = option auto (
  long "scale"
  <> short 's'
  <> help "multiplier for game resolution; must be >= 1"
  <> showDefault
  <> value 1
  <> metavar "DECIMAL"
  )

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
parseCliConfig = CliConfig <$> windowScaleP <*> rendererP
