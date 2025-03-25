{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Arrows             #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module ChoreographyArrow.Choreo where

import ChoreographyArrow.Location
import ChoreographyArrow.Network
import Control.Monad.Freer
import Control.Arrow.Freer.FreerArrowRouter
import Control.Arrow.Freer.Router
import Data.List
import Data.Typeable
import GHC.TypeLits
import Data.Profunctor
import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))
import Control.Arrow.ArrowIO (ArrowIO)

-- * The Choreo monad

-- | A constrained version of `unwrap` that only unwraps values located at a
-- specific location.
type Unwrap l = forall a. a @ l -> a

-- | Effect signature for the `Choreo` monad. @m@ is a monad that represents
-- local computations.
data ChoreoSig ar b a where
  Local :: KnownSymbol l
        => Proxy l
        -> ar b a
        -> ChoreoSig ar (b @ l) (a @ l)

  Comm :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
       => Proxy l
       -> Proxy l'
       -> ChoreoSig ar (a @ l) (a @ l')

instance Show (ChoreoSig ar b a) where
  show (Local l _) = "Local " ++ symbolVal l
  show (Comm l l') = "(" ++ symbolVal l ++ " ~> " ++ symbolVal l' ++ ")"

-- | Monad for writing choreographies.
type Choreo ar = FreerArrow (ChoreoSig ar)

-- | Run a `Choreo` monad directly.
runChoreo :: (Strong ar, Profunctor ar, Arrow ar) => Choreo ar b a -> ar b a
runChoreo = interp handler
  where
    handler :: (Profunctor ar, Arrow ar) => ChoreoSig ar b a -> ar b a
    handler (Local _ ar) = -- wrap <$> m unwrap
      unwrap ^>> ar >>^ wrap

    handler (Comm _ _) = -- return $ (wrap . unwrap) a
      arr (unwrap >>> wrap)

-- | Endpoint projection.
epp :: Choreo ar b a -> LocTm -> Network ar b a
epp c l' = interp handler c
  where
    handler :: ChoreoSig ar b a -> Network ar b a
    handler (Local l ar)
      | toLocTm l == l' = -- wrap <$> run (m unwrap)
          unwrap ^>> run ar >>^ wrap
      | otherwise       = arr (const Empty) -- return Empty
    handler (Comm s r)
      | toLocTm s == toLocTm r = -- return $ wrap (unwrap a)
          arr (wrap . unwrap)
      | toLocTm s == l'        = -- send (unwrap a) (toLocTm r) >> return Empty
          unwrap ^>> send (toLocTm r) >>^ const Empty
      | toLocTm r == l'        = -- wrap <$> recv (toLocTm s)
          const () ^>> recv (toLocTm s) >>^ wrap
      | otherwise              = arr (const Empty) -- return Empty

-- * Choreo operations

discard :: Arrow ar => ar b ()
discard = arr (const ())

-- | Perform a local computation at a given location.
locally :: KnownSymbol l
        => Proxy l           -- ^ Location performing the local computation.
--         -> (Unwrap l -> m a) -- ^ The local computation given a constrained
--                              -- unwrap funciton.
--         -> Choreo m (a @ l)
        -> ar b a
        -> Choreo ar (b @ l) (Tainted (a @ l))
locally l ar = embed (Local l ar)

locally0 :: KnownSymbol l
        => Proxy l           -- ^ Location performing the local computation.
--         -> (Unwrap l -> m a) -- ^ The local computation given a constrained
--                              -- unwrap funciton.
--         -> Choreo m (a @ l)
        -> ar () a
        -> Choreo ar b (Tainted (a @ l))
locally0 l ar = discard >>> arr wrap >>> locally l ar

-- | Communication between a sender and a receiver.
(~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
        => Proxy l
        -> Proxy l'
        -> Choreo ar (a @ l) (Tainted (a @ l'))
(~>) l l' = embed (Comm l l')

-- | A variant of `~>` that sends the result of a local computation.
(~~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
      => (Proxy l, ar b a)
      -> Proxy l'
      -> Choreo ar (b @ l) (Tainted (a @ l'))
(~~>) (l, ar) l' = l `locally` ar >>> clean >>> (l ~> l')

