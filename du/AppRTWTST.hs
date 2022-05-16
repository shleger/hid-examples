{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppRTWTST where

import AppTypes
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

newtype MyApp logEntry state a = MyApp
  { runApp1 ::
      ReaderT
        AppEnv
        ( WriterT
            [logEntry]
            ( StateT
                state
                IO
            )
        )
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader AppEnv,
      MonadWriter [logEntry],
      MonadState state
    )

runMyApp :: MyApp logEntry state a -> AppConfig -> state -> IO (a, [logEntry])
runMyApp app config st =
  evalStateT
    ( runWriterT
        (runReaderT (runApp1 app) (initialEnv config))
    )
    st
