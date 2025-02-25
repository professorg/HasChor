{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import ChoreographyArrow.Choreo
import ChoreographyArrow.Location
import Data.Proxy
import Data.Time
import System.Environment
import System.Random
import Data.Profunctor (Profunctor, Strong, first', second')
import Control.Arrow
import Control.Arrow.ArrowIO
import Control.Category
import Prelude hiding (id, (.))
import Control.Arrow.Freer.FreerArrowChoice
import ChoreographyArrow (runChoreography)
import ChoreographyArrow.Network (Backend)
import ChoreographyArrow.Network.Local
import Data.Bifunctor
import Control.Concurrent.Async (async, mapConcurrently_, wait)

-- set up proxies
alice :: Proxy "alice"
alice = Proxy

bob :: Proxy "bob"
bob = Proxy

discard :: Arrow ar => ar b ()
discard = arr (const ())

optimization :: (ArrowIO ar, Strong ar) => Choreo ar () (Integer @ "alice", Integer @ "alice")
optimization =
  -- wait for alice to initiate the process
  discard >>>
  alice `locally` arr (const (5 :: Integer)) >>>
  (alice ~> bob) >>>
  bob `locally` arr (const (6 :: Integer)) &&& id >>>
  arr fst &&& bob `locally` arr (\(unwrap, p) -> uncurry (+) (bimap unwrap unwrap p)) >>>
  (bob ~> alice) *** (bob ~> alice) >>>
  id &&& alice `locally` (arr (\(unwrap, p) -> bimap unwrap unwrap p) >>> arrIO print) >>>
  arr fst

-- TODO: Should be able to rewrite (bob ~> alice) *** (bob ~> alice) to only perform one send...

main :: IO ()
main = do
  config <- mkLocalConfig locs
  mapConcurrently_ (\l -> runChoreography config optimization l ()) locs
  return ()
  where
    locs = ["alice", "bob"]

