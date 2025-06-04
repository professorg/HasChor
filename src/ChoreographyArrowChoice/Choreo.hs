{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Arrows             #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module ChoreographyArrowChoice.Choreo where

import ChoreographyArrowChoice.Location
import ChoreographyArrowChoice.Network
import Control.Monad.Freer
import Control.Arrow.Freer.FreerChoiceArrow
import Data.List
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Proxy
import GHC.TypeLits
import Data.Profunctor
import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))
import Control.Arrow.ArrowIO (ArrowIO)

-- * The Choreo monad

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

  Cond :: (Show b, Read b, KnownSymbol l)
       => Proxy l
       -> Choreo ar b a
       -> ChoreoSig ar (b @ l) a

instance Show (ChoreoSig ar b a) where
  show (Local l _) = "Local " ++ symbolVal l
  show (Comm l l') = "(" ++ symbolVal l ++ " ~> " ++ symbolVal l' ++ ")"
  show (Cond l c) = "Cond " ++ symbolVal l ++ " >>> " ++ show c

-- | Monad for writing choreographies.
type Choreo ar = FreerChoiceArrow (ChoreoSig ar)

-- This can probably be written generically for any monoid
participants :: Choreo ar b a -> HashSet LocTm
participants (Hom _) = HS.empty
participants (Comp _ e c) =
  participants c <>
  case e of
    Local l _ -> HS.singleton $ symbolVal l
    Comm l l' -> HS.fromList $ [symbolVal l, symbolVal l']
    -- This recursive call is the part that worries me
    Cond l c' -> participants c' <> HS.singleton (symbolVal l)

-- | Run a `Choreo` monad directly.
runChoreo :: (Profunctor ar, ArrowChoice ar) => Choreo ar b a -> ar b a
runChoreo = interp handler
  where
    handler :: (Profunctor ar, ArrowChoice ar) => ChoreoSig ar b a -> ar b a
    handler (Local _ ar) = -- wrap <$> m unwrap
      unwrap ^>> ar >>^ wrap

    handler (Comm _ _) = -- return $ (wrap . unwrap) a
      arr (unwrap >>> wrap)

    handler (Cond _ ar) = -- runChoreo $ c (unwrap a)
      arr unwrap >>> runChoreo ar

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
    handler (Cond l c)
      | toLocTm l == l'  = broadcast p >>> epp c l' -- broadcast >>> epp c l'
      | l' `HS.member` p = const () ^>> recv (toLocTm l) >>> epp c l' -- recv (toLocTm l) >>> epp c l'
      | otherwise        = arr $ const $ error "matching on an unused branch"
      where
        -- We don't want to broadcast to the broadcaster l'
        p = HS.delete (toLocTm l) $ participants c

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
        -> Choreo ar (b @ l) (a @ l)
locally l ar = embed (Local l ar)

locally0 :: KnownSymbol l
        => Proxy l           -- ^ Location performing the local computation.
--         -> (Unwrap l -> m a) -- ^ The local computation given a constrained
--                              -- unwrap funciton.
--         -> Choreo m (a @ l)
        -> ar () a
        -> Choreo ar b (a @ l)
locally0 l ar = discard >>> arr wrap >>> locally l ar


-- | Communication between a sender and a receiver.
(~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
        => Proxy l
        -> Proxy l'
        -> Choreo ar (a @ l) (a @ l')
(~>) l l' = embed (Comm l l')

--TODO
-- -- | Conditionally execute choreographies based on a located value.
cond :: (Show b, Read b, KnownSymbol l)
     => Proxy l
     -> Choreo ar b a
     -> Choreo ar (b @ l) a
cond l c = embed (Cond l c)

-- | A variant of `~>` that sends the result of a local computation.
(~~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
      => (Proxy l, ar b a)
      -> Proxy l'
      -> Choreo ar (b @ l) (a @ l')
(~~>) (l, ar) l' = l `locally` ar >>> (l ~> l')

-- -- | A variant of `cond` that conditonally executes choregraphies based on the
-- -- result of a local computation.
cond' :: (Show x, Read x, KnownSymbol l)
      => Proxy l
      -> ar b x
      -> Choreo ar x a
      -> Choreo ar (b @ l) a
cond' l ar c = l `locally` ar >>> cond l c

