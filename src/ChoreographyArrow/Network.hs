-- | This module defines the `Network` monad, which represents programs run on
-- individual nodes in a distributed system with explicit sends and receives.
-- To run a `Network` program, we provide a `runNetwork` function that supports
-- multiple message transport backends.
module ChoreographyArrow.Network where

import ChoreographyArrow.Location
import Control.Monad.Freer
import Control.Monad.IO.Class
import Control.Arrow.FreerArrow

-- * The Network monad

-- | Effect signature for the `Network` monad.
data NetworkSig ar b a where
  -- | Local computation.
  Run :: ar b a
      -> NetworkSig ar b a
  -- | Sending.
  Send :: Show a
       => LocTm
       -> NetworkSig ar a ()
  -- | Receiving.
  Recv :: Read a
       => LocTm
       -> NetworkSig ar () a
  -- | Broadcasting.
  BCast :: Show a
        => NetworkSig ar a ()

-- | Monad that represents network programs.
type Network ar = FreerArrow (NetworkSig ar)

-- * Network operations

-- | Perform a local computation.
run :: ar b a -> Network ar b a
run ar = embed $ Run ar

-- | Send a message to a receiver.
send :: Show a => LocTm -> Network ar a ()
send l = embed $ Send l

-- | Receive a message from a sender.
recv :: Read a => LocTm -> Network ar () a
recv l = embed $ Recv l

-- | Broadcast a message to all participants.
broadcast :: Show a => Network ar a ()
broadcast = embed BCast

-- * Message transport backends

-- | A message transport backend defines a /configuration/ of type @c@ that
-- carries necessary bookkeeping information, then defines @c@ as an instance
-- of `Backend` and provides a `runNetwork` function.

--TODO
--class Backend c where
--  runNetwork :: MonadIO m => c -> LocTm -> Network m a -> m a
