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
import Data.Maybe
import Data.Typeable
import Data.Time
import System.Environment
import System.Random
import Data.Profunctor ( Strong, Profunctor(lmap) )
import Control.Arrow
import Control.Arrow.ArrowIO
import Control.Category
import Control.Monad
import Prelude hiding (id, (.))
import Control.Arrow.Freer.FreerArrow
import ChoreographyArrow
import ChoreographyArrow.Network
import ChoreographyArrow.Network.Local
import qualified Data.Bifunctor as B
import Control.Concurrent.Async (async, mapConcurrently_, wait)
import GHC.TypeLits
import Data.Tuple
import Data.Bitraversable

-- set up proxies
alice :: Proxy "alice"
alice = Proxy

bob :: Proxy "bob"
bob = Proxy

discard :: Arrow ar => ar b ()
discard = arr (const ())

choreo :: (ArrowIO ar, Strong ar) => Choreo ar () (Integer @ "alice", Integer @ "alice")
choreo =
  -- wait for alice to initiate the process
  discard >>>
  alice `locally` arr (const (5 :: Integer)) >>>
  (alice ~> bob) >>>
  bob `locally` arr (const (6 :: Integer)) &&& id >>>
  arr fst &&& bob `locally` arr (\(unwrap, p) -> uncurry (+) (B.bimap unwrap unwrap p)) >>>
  (bob ~> alice) *** (bob ~> alice) >>>
  id &&& alice `locally` (arr (\(unwrap, p) -> B.bimap unwrap unwrap p) >>> arrIO print) >>>
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

choreo_IO :: Choreo (Kleisli IO) () (Integer @ "alice", Integer @ "alice")
choreo_IO = choreo

choreo_epp :: (ArrowIO ar, Strong ar) => LocTm -> Network ar () (Integer @ "alice", Integer @ "alice")
choreo_epp = epp choreo

choreo_epp_IO :: LocTm -> Network (Kleisli IO) () (Integer @ "alice", Integer @ "alice")
choreo_epp_IO = choreo_epp

choreo_run_IO :: Kleisli IO () (Integer @ "alice", Integer @ "alice")
choreo_run_IO = runChoreo choreo

distr_loc :: KnownSymbol l => Unwrap l -> (a, b) @ l -> (a @ l, b @ l)
distr_loc unwrap = unwrap >>> B.bimap wrap wrap

factor_loc :: KnownSymbol l => Unwrap l -> (a @ l, b @ l) -> (a, b)
factor_loc unwrap = B.bimap unwrap unwrap

distr_unwrap :: KnownSymbol l => (Unwrap l, (a, b)) -> ((Unwrap l, a), (Unwrap l, b))
distr_unwrap (uw, (a, b)) = ((uw, a), (uw, b))

setup_err :: a
setup_err = error $ "Dummy value"

newtype AtLoc (l :: LocTy) a = AtLoc { unLoc :: a @ l }

instance KnownSymbol l => Functor (AtLoc l) where
  fmap f (AtLoc (Wrap x)) = AtLoc (Wrap (f x))
  fmap f (AtLoc Empty) = AtLoc Empty

instance KnownSymbol l => Applicative (AtLoc l) where
  pure = AtLoc . Wrap
  (<*>) = ap

instance KnownSymbol l => Monad (AtLoc l) where
  return = pure
  (AtLoc (Wrap x)) >>= k = k x
  (AtLoc Empty) >>= k = AtLoc Empty


optimize' :: Arrow ar => Choreo ar a b -> (Choreo ar a b, Bool)

optimize' (Hom f) = (Hom f, False)

-- This isn't possible with the current types
-- optimize' (Comp f (Local (l :: Proxy l) c) (Comp g (Local (l' :: Proxy l') d) k))
--   | isJust (eqT @l @l') =
--     case (eqT @l @l') of
--       Just Refl ->
--         (,True) $
--         optimize $
--         Comp
--           (f >>> arr (,()))
--           (Local l (
--               arr distr_unwrap >>>
--               ((c >>> arr wrap) *** arr swap) >>>
--               arr unassoc >>>
--               first (arr g) >>>
--               arr (swap >>> unassoc) >>>
--               first d
--           )) $
--         lmap _ $
--         k

optimize' (Comp f (Comm (l :: Proxy l) (l' :: Proxy l'))
          (Comp g (Comm (m :: Proxy m) (m' :: Proxy m'))
           k))
  | isJust (eqT @l @m) && isJust (eqT @l' @m') =
    case (eqT @l @m, eqT @l' @m') of
      (Just Refl, Just Refl) ->
        (,True) $
        optimize $
        Comp (
          f >>>
          (\(a2, c) -> (unLoc $ bisequence (AtLoc a2, AtLoc $ fst $ g (setup_err, c)), c))
        ) (Comm l l') $
        lmap (
          (\(p, c) -> ((unLoc $ fst <$> AtLoc p, unLoc $ snd <$> AtLoc p), c)) >>>
          (\((a2, a4), c) -> (g (a2, c), a4)) >>>
          (\((_, c), a4) -> (a4, c))
        ) $
        k

optimize' (Comp f e c) =
  let
    c' = optimize' c
    (d, b) = c'
  in
    if b then
      optimize' $ Comp f e d
    else
      (,False) $ Comp f e d

optimize :: Arrow ar => Choreo ar a b -> Choreo ar a b
optimize = fst . optimize'

choreo_opt :: (ArrowIO ar, Strong ar) => Choreo ar () (Integer @ "alice", Integer @ "alice")
choreo_opt = optimize choreo

choreo_opt_IO :: Choreo (Kleisli IO) () (Integer @ "alice", Integer @ "alice")
choreo_opt_IO = choreo_opt

choreo_opt_epp :: (ArrowIO ar, Strong ar) => LocTm -> Network ar () (Integer @ "alice", Integer @ "alice")
choreo_opt_epp = epp choreo_opt

choreo_opt_epp_IO :: LocTm -> Network (Kleisli IO) () (Integer @ "alice", Integer @ "alice")
choreo_opt_epp_IO = choreo_opt_epp

choreo_opt_run_IO :: Kleisli IO () (Integer @ "alice", Integer @ "alice")
choreo_opt_run_IO = runChoreo choreo_opt

main' :: Choreo (Kleisli IO) () (Integer @ "alice", Integer @ "alice") -> IO ()
main' c = do
  config <- mkLocalConfig locs
  mapConcurrently_ (\l -> runChoreography config c l ()) locs
  return ()
  where
    locs = ["alice", "bob"]

main :: IO ()
main = main' choreo

