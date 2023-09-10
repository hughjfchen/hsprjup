-- | This module define the Env for application

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}

module AppEnv (
  Command(..)
  , AppEnv(..)
  ) where

import Has

import Core.Types

data Command = Start
        | Stop
        | Status
        | Unknown
        deriving stock (Eq, Show, Read, Generic)

-- | following is just for example
-- | you must put your own env
data AppEnv = AppEnv {
  appEnvCommand :: Command
  } deriving stock (Generic)
        deriving (Has Hsprjup) via Field "appEnvHsprjup" AppEnv

