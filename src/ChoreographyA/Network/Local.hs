{-# LANGUAGE GADTs #-}

-- | This module defines the multi-thread backend for the `Network` monad.
module ChoreographyA.Network.Local where

import ChoreographyA.Location
import ChoreographyA.Network
import Control.Applicative.Free
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
-- import Control.Monad.Freer
import Control.Monad.IO.Class
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HashMap
import Control.Applicative.IO
import Data.Functor

-- | Each location is associated with a message buffer which stores messages sent
-- from other locations.
type MsgBuf = HashMap LocTm (Chan String)

newtype LocalConfig = LocalConfig
  { locToBuf :: HashMap LocTm MsgBuf
  }

newEmptyMsgBuf :: [LocTm] -> IO MsgBuf
newEmptyMsgBuf = foldM f HashMap.empty
  where
    f hash loc = do
      chan <- newChan
      return (HashMap.insert loc chan hash)

mkLocalConfig :: [LocTm] -> IO LocalConfig
mkLocalConfig locs = LocalConfig <$> foldM f HashMap.empty locs
  where
    f hash loc = do
      buf <- newEmptyMsgBuf locs
      return (HashMap.insert loc buf hash)

locs :: LocalConfig -> [LocTm]
locs = HashMap.keys . locToBuf

runNetworkLocal :: MonadIO f => LocalConfig -> LocTm -> NetworkA f a -> f a
runNetworkLocal cfg self prog = runAp handler prog
  where
    handler :: MonadIO f => NetworkSigA f a -> f a
    handler (Run m)    = m
    handler (Send l) = -- writeChan ((locToBuf cfg ! l) ! self) (show a)
      flip fmap show <$> liftIO
        (writeChanA ((locToBuf cfg ! l) ! self))
    handler (Recv l)   = liftIO $ read <$> readChan ((locToBuf cfg ! self) ! l)
    handler BCast  = -- mapM_ handler $ fmap (Send a) (locs cfg)
      let locs' = locs cfg in
      let locs'' = fmap Send (locs cfg) in
      foldr (\x y -> y *> handler x) (pure (const ())) locs''

instance Backend LocalConfig where
  runNetwork = runNetworkLocal

