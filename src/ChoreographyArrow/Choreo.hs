{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Arrows             #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module ChoreographyArrow.Choreo where

import ChoreographyArrow.Location
import ChoreographyArrow.Network
import Control.Monad.Freer
import Control.Arrow.FreerArrow
import Data.List
import Data.Proxy
import GHC.TypeLits
import Data.Profunctor (Profunctor)
import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))

-- * The Choreo monad

-- | A constrained version of `unwrap` that only unwraps values located at a
-- specific location.
type Unwrap l = forall a. a @ l -> a

-- | Effect signature for the `Choreo` monad. @m@ is a monad that represents
-- local computations.
data ChoreoSig ar b a where
  Local :: KnownSymbol l
        => Proxy l
        -> ar (Unwrap l, b) a
        -> ChoreoSig ar b (a @ l)

  Comm :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
       => Proxy l
       -> Proxy l'
       -> ChoreoSig ar (a @ l) (a @ l')
--TODO
--   Cond :: (Show a, Read a, KnownSymbol l)
--        => Proxy l
--        -> a @ l
--        -> (a -> Choreo m b)
--        -> ChoreoSig m b

-- | Monad for writing choreographies.
type Choreo ar = FreerArrow (ChoreoSig ar)

-- | Run a `Choreo` monad directly.
runChoreo :: (Profunctor ar, Arrow ar) => Choreo ar b a -> ar b a
runChoreo = interp handler
  where
    handler :: (Profunctor ar, Arrow ar) => ChoreoSig ar b a -> ar b a
    handler (Local _ ar) = -- wrap <$> m unwrap
      arr (\x -> (unwrap, x)) >>> ar >>> arr wrap

    handler (Comm _ _) = -- return $ (wrap . unwrap) a
      arr (wrap . unwrap)

--     handler (Cond _ a c) = runChoreo $ c (unwrap a)

-- | Endpoint projection.
epp :: Choreo ar b a -> LocTm -> Network ar b a
epp c l' = interp handler c
  where
    handler :: ChoreoSig ar b a -> Network ar b a
    handler (Local l ar)
      | toLocTm l == l' = -- wrap <$> run (m unwrap)
          arr (\x -> (unwrap, x)) >>> run ar >>> arr wrap
      | otherwise       = arr (const Empty) -- return Empty
    handler (Comm s r)
      | toLocTm s == toLocTm r = -- return $ wrap (unwrap a)
          arr (wrap . unwrap)
      | toLocTm s == l'        = -- send (unwrap a) (toLocTm r) >> return Empty
          arr unwrap >>> send (toLocTm r) >>^ const Empty
      | toLocTm r == l'        = -- wrap <$> recv (toLocTm s)
          const () ^>> recv (toLocTm s) >>> arr wrap
      | otherwise              = arr (const Empty) -- return Empty
--TODO
--     handler (Cond l a c)
--       | toLocTm l == l' = broadcast (unwrap a) >> epp (c (unwrap a)) l'
--       | otherwise       = recv (toLocTm l) >>= \x -> epp (c x) l'
-- 
-- * Choreo operations

-- | Perform a local computation at a given location.
locally :: KnownSymbol l
        => Proxy l           -- ^ Location performing the local computation.
--         -> (Unwrap l -> m a) -- ^ The local computation given a constrained
--                              -- unwrap funciton.
--         -> Choreo m (a @ l)
        -> ar (Unwrap l, b) a
        -> Choreo ar b (a @ l)
locally l ar = embed (Local l ar)

-- | Communication between a sender and a receiver.
(~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
        => Proxy l
        -> Proxy l'
        -> Choreo ar (a @ l) (a @ l')
(~>) l l' = embed (Comm l l')

--TODO
-- -- | Conditionally execute choreographies based on a located value.
-- cond :: (Show a, Read a, KnownSymbol l)
--      => (Proxy l, a @ l)  -- ^ A pair of a location and a scrutinee located on
--                           -- it.
--      -> (a -> Choreo m b) -- ^ A function that describes the follow-up
--                           -- choreographies based on the value of scrutinee.
--      -> Choreo m b
-- cond (l, a) c = toFreer (Cond l a c)

-- | A variant of `~>` that sends the result of a local computation.
(~~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
      => (Proxy l, ar (Unwrap l, b) a)
      -> Proxy l'
      -> Choreo ar b (a @ l')
(~~>) (l, ar) l' = l `locally` ar >>> (l ~> l')

-- -- | A variant of `cond` that conditonally executes choregraphies based on the
-- -- result of a local computation.
-- cond' :: (Show a, Read a, KnownSymbol l)
--       => (Proxy l, Unwrap l -> m a) -- ^ A pair of a location and a local
--                                     -- computation.
--       -> (a -> Choreo m b)          -- ^ A function that describes the follow-up
--                                     -- choreographies based on the result of the
--                                     -- local computation.
--       -> Choreo m b
-- cond' (l, m) c = do
--   x <- l `locally` m
--   cond (l, x) c
