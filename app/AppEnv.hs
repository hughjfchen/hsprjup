{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

-- | This module define the Env for application
module AppEnv
  ( Command (..),
    AppEnv (..),
  )
where

import Core.Types
import Has

data Command
  = Start
  | Stop
  | Status
  | Unknown
  deriving stock (Eq, Show, Read, Generic)

-- | following is just for example
-- | you must put your own env
data AppEnv = AppEnv
  { appEnvCommand :: Command
  }
  deriving stock (Generic)
  deriving (Has Command) via Field "appEnvCommand" AppEnv
