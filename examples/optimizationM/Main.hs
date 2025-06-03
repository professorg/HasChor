{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}

module Main where

import Choreography.Choreo
import Choreography.Location
import Data.Maybe
import Data.Time
import System.Environment
import System.Random
import Control.Monad
import Prelude hiding (id, (.))
import Choreography
import Choreography.Network
import Choreography.Network.Local
import Control.Concurrent.Async (async, mapConcurrently_, wait)
import GHC.TypeLits
import Data.Tuple
import Data.Proxy
import Data.Bitraversable
import Control.Monad.IO.Class

-- set up proxies
alice :: Proxy "alice"
alice = Proxy

bob :: Proxy "bob"
bob = Proxy

choreo :: MonadIO m => Choreo m (Integer @ "alice", Integer @ "alice")
choreo = do
  xa <- alice `locally` (\_ -> pure (5 :: Integer))
  xb <- (alice, xa) ~> bob
  yb <- bob `locally` (\_ -> pure (6 :: Integer))
  zb <- bob `locally` (\unwrap -> pure $ unwrap xb + unwrap yb)
  za <- (bob, zb) ~> alice
  ya <- (bob, yb) ~> alice
  alice `locally` (\unwrap -> liftIO $ print $ (unwrap za, unwrap ya))
  pure (za, ya)

main :: IO ()
main = do
  [loc] <- getArgs
  x <- case loc of
    "alice" -> runChoreography config choreo "alice"
    "bob" -> runChoreography config choreo "bob"
  return ()
  where
    config =
      mkHttpConfig
        [ ("alice", ("localhost", 5000)),
          ("bob", ("localhost", 5001))
        ]

