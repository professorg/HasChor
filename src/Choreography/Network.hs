{-# LANGUAGE GADTs #-}

--------------------------------------------------------------------------------
-- This module provides the `Network` monad, which represents programs run on
-- individual nodes in a distributed system with explicit sends and receives.
--------------------------------------------------------------------------------

module Choreography.Network where

import Choreography.Location
import Control.Concurrent.Chan
import Data.HashMap.Strict (HashMap, (!), insert, empty)
import Control.Monad (foldM)
import Control.Monad.Freer

data NetworkF a where
  Send :: Show a => a -> Location -> NetworkF ()
  Recv :: Read a => Location -> NetworkF a

type Network = Freer (IO :+: NetworkF)

send :: Show a => a -> Location -> Network ()
send a l = toFreer $ Inr (Send a l)

recv :: Read a => Location -> Network a
recv l = toFreer $ Inr (Recv l)

doIO :: IO a -> Network a
doIO io = toFreer $ Inl io

data Context = Context
  { sendChan :: Chan (Location, String)
  , recvChans :: HashMap Location (Chan String)
  }

mkContext :: [Location] -> IO Context
mkContext ls = do
  recvChans <- foldM f empty ls
  sendChan <- newChan
  return $ Context { recvChans = recvChans, sendChan = sendChan }
  where
    f :: HashMap Location (Chan String) -> Location
      -> IO (HashMap Location (Chan String))
    f hm l = do
      c <- newChan
      return $ insert l c hm

-- interpet a `Network` program as a `IO` program with access to a `Context`
-- how `Context` is manipulated is determined in the next phase, this allows
-- us to have multiple implementations of `Network` while reusing most of the
-- code
interpNetwork :: Context -> Network a -> IO a
interpNetwork ctx = runFreer f
  where
    f :: (IO :+: NetworkF) a -> IO a
    f (Inl io)         = io
    f (Inr (Send a l)) = writeChan (sendChan ctx) (l, show a)
    f (Inr (Recv l))   = read <$> readChan (recvChans ctx ! l)