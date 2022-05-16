{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module AppRWST where

import AppTypes
import Control.Monad.RWS

type MyApp logEntry state = RWST AppEnv [logEntry] state IO

runMyApp :: MyApp logEntry state a -> AppConfig -> state -> IO (a, [logEntry])
runMyApp app config st = evalRWST app (initialEnv config) st
