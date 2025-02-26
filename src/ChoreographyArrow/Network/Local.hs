{-# LANGUAGE GADTs #-}

-- | This module defines the multi-thread backend for the `Network` monad.
module ChoreographyArrow.Network.Local where

import ChoreographyArrow.Location
import ChoreographyArrow.Network
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HashMap
import Control.Arrow.ArrowIO
import Control.Arrow.Freer.FreerArrowChoiceL
import Data.Profunctor (Profunctor)
import Control.Arrow

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

runNetworkLocal :: MonadIO m => LocalConfig -> LocTm -> Network (Kleisli m) b a -> b -> m a
runNetworkLocal cfg self prog = runKleisli $ interp handler prog
  where
    handler :: MonadIO m => NetworkSig (Kleisli m) b a -> Kleisli m b a
    handler (Run ar) = ar
    handler (Send l) = arrIO (\a -> liftIO $ writeChan ((locToBuf cfg ! l) ! self) (show a))
    handler (Recv l) = arrIO0 $ liftIO $ read <$> readChan ((locToBuf cfg ! self) ! l)
    handler BCast   = -- mapM_ handler $ fmap Send (locs cfg)
      Kleisli $ \x -> mapM_ (\l -> runKleisli (handler $ Send l) x) (locs cfg)

instance Backend LocalConfig where
  runNetwork = runNetworkLocal

