{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

import Control.Monad.Reader

data Config = Config
  { verbose :: Bool
  {- other parameters -}
  }

type ConfigM = Reader Config

getConfiguration :: IO Config
getConfiguration = pure Config {verbose = True {- ... -}}

main :: IO ()
main = do
  config <- getConfiguration
  let result = runReader work config
  print result
  print (runReader r1 "ddd")
  print runJerryRun

work :: ConfigM ()
work = do
  -- ...
  doSomething

-- ...

doSomething :: ConfigM ()
doSomething = do
  -- ...
  doSomethingSpecial

-- ...

doSomethingSpecial :: ConfigM ()
doSomethingSpecial = do
  -- ...
  -- Config {verbose} <- ask
  vrb <- asks verbose
  when vrb beVerbose

-- ...

beVerbose :: ConfigM ()
beVerbose = pure ()

silent :: Config -> Config
silent config = config {verbose = False}

doSomethingSpecialSilently :: ConfigM ()
doSomethingSpecialSilently = local silent doSomethingSpecial

-- extended article on subj: https://engineering.dollarshaveclub.com/the-reader-monad-example-motivation-542c54ccfaa8
-- https://blog.ssanj.net/posts/2014-09-23-A-Simple-Reader-Monad-Example.html
r1 :: Reader String Int
r1 = pure 5 :: Reader String Int

tom :: Reader String String
tom = do
  env <- ask -- gives you the environment which in this case is a String
  pure (env ++ " This is Tom.") -- == return

jerry :: Reader String String
jerry = do
  env <- ask
  return (env ++ " This is Jerry.")

tomAndJerry :: Reader String String
tomAndJerry = do
  t <- tom
  j <- jerry
  return (t ++ "\n" ++ j)

runJerryRun :: String
runJerryRun = runReader tomAndJerry "Who is this?"
