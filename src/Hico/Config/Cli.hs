{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hico.Config.Cli where

import           Data.Maybe
import           Data.Semigroup      ((<>))
import           Hico.Types
import           Options.Applicative
import           SDL                 (RendererType (AcceleratedRenderer, AcceleratedVSyncRenderer, SoftwareRenderer, UnacceleratedRenderer),
                                      defaultRenderer, rendererType)

data CliConfig = CliConfig {
  windowScale :: Float,
  renderer    :: Maybe RendererType
}

widthBaseCfg  = 320
heightBaseCfg = 240

processRunConfig :: CliConfig -> GameConfig
processRunConfig (CliConfig wS _) | wS < 1  = error "Window Scale must be >= 1"
processRunConfig raw = GameConfig {
  widthBase    = widthBaseCfg
  , heightBase = heightBaseCfg
  , width      = floor (fromIntegral widthBaseCfg  * windowScale raw)
  , height     = floor (fromIntegral heightBaseCfg * windowScale raw)
  , renderer   = fromMaybe defaultRendererType (rendererRaw raw)
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

defaultRendererP :: Parser RendererType
defaultRendererP = flag' defaultRendererType (
  long "default_rndr"
  <> help "Use SDL's default renderer")

acceleratedRendererP :: Parser RendererType
acceleratedRendererP = flag' AcceleratedRenderer (
  long "accelerated_rndr"
  <> help "Use SDL's accelerated renderer")

acceleratedVSyncRendererP :: Parser RendererType
acceleratedVSyncRendererP = flag' AcceleratedVSyncRenderer (
  long "accelerated_vsync_rndr"
  <> help "Use SDL's accelerated renderer with vsync")

softwareRendererP :: Parser RendererType
softwareRendererP = flag' SoftwareRenderer (
  long "software_rndr"
  <> help "Use SDL's default renderer")

unacceleratedRendererP :: Parser RendererType
unacceleratedRendererP = flag' UnacceleratedRenderer (
  long "unaccelerated_rndr"
  <> help "Use SDL's unaccelerated renderer")

rendererP :: Parser (Maybe RendererType)
rendererP = optional (
  acceleratedRendererP <|>
  acceleratedVSyncRendererP <|>
  defaultRendererP  <|>
  softwareRendererP <|>
  unacceleratedRendererP
  )

parseCliConfig :: Parser CliConfig
parseCliConfig = CliConfig <$> windowScaleP <*> rendererP
