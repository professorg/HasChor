{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ImpredicativeTypes   #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-- | This module defines `ChoreoA`, the applicative functor for writing choreographies.
module ChoreographyA.Choreo where

import ChoreographyA.Location
import ChoreographyA.Network
import Control.Applicative.Free
import Control.Selective.Free
import Data.Functor
import Data.List
import Data.Proxy
import GHC.TypeLits
import Data.Type.Equality (inner)
import Servant (runHandler)

-- * The ChoreoA applicative functor

-- | A constrained version of `unwrap` that only unwraps values located at a
-- specific location.
type Unwrap l = forall a. a @ l -> a

-- | Effect signature for the `ChoreoA` applicative functor. @f@ is an applicative functor that represents
-- local computations.
data ChoreoSigA f a where
  Local :: (KnownSymbol l)
        => Proxy l
        -> f (Unwrap l -> a)
        -> ChoreoSigA f (a @ l)

  Comm :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
       => Proxy l
       -> Proxy a
       -> Proxy l'
       -> ChoreoSigA f (a @ l -> a @ l')

  Cond :: (Show a, Read a, KnownSymbol l)
       => Proxy l
       -> Proxy a
       -> (ChoreoA f (a -> b))
       -> ChoreoSigA f (a @ l -> b)

-- | Applicative functor for writing choreographies.
type ChoreoA f = Ap (ChoreoSigA f)

type ChoreoAS f = Select (ChoreoSigA f)

-- | Run a `ChoreoA` applicative functor directly.
runChoreoA :: Applicative f => ChoreoA f a -> f a
runChoreoA = runAp handler
  where
    handler :: Applicative f => ChoreoSigA f a -> f a
    handler (Local _ g)  = wrap <$> (g <*> pure unwrap) -- wrap <$> (g <*> pure unwrap)
    handler (Comm {}) = pure $ wrap . unwrap
    handler (Cond _ _ c) = -- runChoreoA $ c (unwrap a)
      let c' = c <&> (unwrap <&>) in
      runChoreoA c'

-- | Endpoint projection.
epp :: ChoreoA f a -> LocTm -> NetworkA f a
epp c l' = runAp handler c
  where
    handler :: ChoreoSigA f a -> NetworkA f a
    handler (Local l g)
      | toLocTm l == l' = wrap <$> (run g <*> pure unwrap)
      | otherwise       = pure Empty

    handler (Comm s a r)
      | toLocTm s == toLocTm r = pure $ wrap . unwrap
      | toLocTm s == l'        = -- send (unwrap a) (toLocTm r) *> return Empty
        send a (toLocTm r)
          <&> (unwrap <&>)
          $> const Empty
      | toLocTm r == l'        = -- wrap <$> recv (toLocTm s)
        const <$> (wrap <$> recv a (toLocTm s))
      | otherwise              = pure $ const Empty

    handler (Cond l a c)
      | toLocTm l == l' = -- broadcast (unwrap a) >> epp (c (unwrap a)) l'
        (broadcast a <&> (unwrap <&>))
          *> epp
              (c <&> (unwrap <&>))
              l'
      | otherwise       = -- recv (toLocTm l) >>= \x -> epp (c x) l'
        const <$> (epp c l' <*> recv a (toLocTm l))
        -- recv a (toLocTm l) <*> epp c l'

-- * ChoreoA operations

-- | Perform a local computation at a given location.
locally :: KnownSymbol l
        => Proxy l           -- ^ Location performing the local computation.
        -> f (Unwrap l -> a) -- ^ The local computation given a constrained
                             -- unwrap funciton.
        -> ChoreoA f (a @ l)
locally l g = liftAp (Local l g)

-- | Communication between a sender and a receiver.
(~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
     => Proxy l  -- ^ A pair of a sender's location and a value located
                          -- at the sender
     -> Proxy l'          -- ^ A receiver's location.
     -> ChoreoA f (a @ l -> a @ l')
(~>) l l' = liftAp (Comm l Proxy l')

-- | Conditionally execute choreographies based on a located value.
cond :: (Show a, Read a, KnownSymbol l)
     => Proxy l  -- ^ A pair of a location and a scrutinee located on
                          -- it.
     -> ChoreoA f (a -> b) -- ^ A function that describes the follow-up
                          -- choreographies based on the value of scrutinee.
     -> ChoreoA f (a @ l -> b)
cond l c = liftAp (Cond l Proxy c)

-- | A variant of `~>` that sends the result of a local computation.
(~~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
      => (Proxy l, f (Unwrap l -> a)) -- ^ A pair of a sender's location and a local
                                    -- computation.
      -> Proxy l'                   -- ^ A receiver's location.
      -> ChoreoA f (a @ l')
(~~>) (l, g) l' =
  l ~> l' <*> locally l g
  

-- do
--   x <- l `locally` f
--   (l, x) ~> l'

-- | A variant of `cond` that conditonally executes choregraphies based on the
-- result of a local computation.
cond' :: (Show a, Read a, KnownSymbol l)
      => (Proxy l, f (Unwrap l -> a)) -- ^ A pair of a location and a local
                                    -- computation.
      -> ChoreoA f(a -> b)          -- ^ A function that describes the follow-up
                                    -- choreographies based on the result of the
                                    -- local computation.
      -> ChoreoA f b
cond' (l, g) c =
  cond l c <*> locally l g
-- do
--   x <- l `locally` f
--   cond (l, x) c
