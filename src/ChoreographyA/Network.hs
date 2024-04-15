-- | This module defines the `Network` applicative functor, which represents programs run on
-- individual nodes in a distributed system with explicit sends and receives.
-- To run a `Network` program, we provide a `runNetwork` function that supports
-- multiple message transport backends.
module ChoreographyA.Network where

import Choreography.Location
import Control.Applicative
import Control.Applicative.Free
import Data.Composition
import Data.Functor.Compose
import Data.Functor.Product
import Data.Proxy
import Control.Monad.IO.Class

-- * The NetworkA applicative function

-- | Effect signature for the `Network` applicative functor.
data NetworkSigA f a where
  -- | Local computation.
  Run :: f a
      -> NetworkSigA f a
  -- | Sending.
  Send :: Show a
       => LocTm
       -> NetworkSigA f (a -> ())
  -- | Receiving.
  Recv :: Read a
       => LocTm
       -> NetworkSigA f a
  -- | Broadcasting.
  BCast :: Show a
        => NetworkSigA f (a -> ())

-- | Applicative functor that represents network programs.
type NetworkA f = Ap (NetworkSigA f)

-- * NetworkA operations

-- | Perform a local computation.
run :: f a -> NetworkA f a
run f = liftAp $ Run f

-- | Send a message to a receiver.
send :: Show a => Proxy a -> LocTm -> NetworkA f (a -> ())
send _ l = liftAp $ Send l

-- | Receive a message from a sender.
recv :: Read a => Proxy a -> LocTm -> NetworkA f a
recv _ l = liftAp $ Recv l

-- | Broadcast a message to all participants.
broadcast :: Show a => Proxy a -> NetworkA f (a -> ())
broadcast _ = liftAp BCast

-- * Message transport backends

class (Applicative f) => ApplicativeIO f where
  liftIOA :: IO a -> f a

instance ApplicativeIO IO where
  liftIOA = id

instance (Applicative f) => ApplicativeIO (Compose IO f) where
  liftIOA = Compose . fmap pure

instance (Applicative f) => ApplicativeIO (Compose f IO) where
  liftIOA = Compose . pure

-- | A message transport backend defines a /configuration/ of type @c@ that
-- carries necessary bookkeeping information, then defines @c@ as an instance
-- of `Backend` and provides a `runNetwork` function.
class Backend c where
  runNetwork :: ApplicativeIO m => c -> LocTm -> NetworkA f a -> m a

