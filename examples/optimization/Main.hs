{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}

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
import Control.Arrow.Freer.FreerArrowChoiceL
import ChoreographyArrow (runChoreography)
import ChoreographyArrow.Network
import ChoreographyArrow.Network.Local
import Data.Bifunctor
import Control.Concurrent.Async (async, mapConcurrently_, wait)
import GHC.TypeLits

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
-- (bob ~> alice) *** (bob ~> alice)
--  =
-- (bob ~> alice) >>> (arr (\(unwrap, p) -> let (x, y) = (unwrap p) in (wrap x, wrap y)))
--
-- (a @ "alice", b @ "alice") = ((a, b) @ "alice")
--
-- first (bob ~> alice) >>>
-- arr (\(x, y) -> (y, x)) >>>
-- first (bob ~> alice)

--TODO: Write different interpretations paths for this

-- (Choreo ar ~> Kleisli IO (Network ar))

optimization_epp :: (ArrowIO ar, Strong ar) => LocTm -> Network ar () (Integer @ "alice", Integer @ "alice")
optimization_epp l = epp optimization l

optimization_run_IO :: Kleisli IO () (Integer @ "alice", Integer @ "alice")
optimization_run_IO = runChoreo optimization

combine_local :: Choreo ar a b -> Choreo ar a b
combine_local (Hom f) = Hom f
combine_local (Comp f (Local l c) (Comp g (Local l' d) k))
  | symbolVal l == symbolVal l'   = _ -- TODO: I don't know if there's a valid definition that can go here
  | otherwise = (Comp f (Local l c) (Comp g (Local l' d) k))
combine_local (Comp f e c) = Comp f e (combine_local c)

main :: IO ()
main = do
  config <- mkLocalConfig locs
  mapConcurrently_ (\l -> runChoreography config optimization l ()) locs
  return ()
  where
    locs = ["alice", "bob"]

