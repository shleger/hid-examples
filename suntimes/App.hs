{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module App where

import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Types

newtype MyApp a = MyApp {runApp :: ReaderT WebAPIAuth (LoggingT IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader WebAPIAuth,
      MonadLogger
    )

runMyApp :: MyApp a -> WebAPIAuth -> IO a
runMyApp app config = runStderrLoggingT (runReaderT (runApp app) config) --add runner for the logger
