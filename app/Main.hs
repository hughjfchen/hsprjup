
module Main (main) where

import AppEnv
import AppM
import Core.Types
import AppCapability.Hsprjup()
import Core.Hsprjup

main :: IO ()
main = do
  appEnv <- AppEnv { appEnvCommand = Start }

  case appEnvCommand appEnv of
    Start -> defaultStartCommand
    Stop -> defaultStopCommand
    Status -> defaultStatusCommand
    Unknown -> error "Unknown command, it must be start, stop or status."
  where
    defaultStartCommand = putStrLn "Please implement the start command" 
    defaultStopCommand = putStrLn "Please implement the stop command" 
    defaultStatusCommand = putStrLn "Please implement the status command" 
