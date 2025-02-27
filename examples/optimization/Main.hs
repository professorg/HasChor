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
import Prelude hiding (id, (.))
import Control.Arrow.Freer.FreerArrowL
import ChoreographyArrow
import ChoreographyArrow.Network
import ChoreographyArrow.Network.Local
import qualified Data.Bifunctor as B
import Control.Concurrent.Async (async, mapConcurrently_, wait)
import GHC.TypeLits
import Data.Tuple

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

optimize' :: Arrow ar => Choreo ar a b -> (Choreo ar a b, Bool)

optimize' (Hom f) = (Hom f, False)
-- optimize' (Comp f (Local (l :: Proxy l) c) (Comp g (Local (l' :: Proxy l') d) k))
--   | isJust (eqT @l @l') =
--     case (eqT @l @l') of
--       Just Refl ->
--         (,True) $
--         fst . optimize' $
--         Comp
--           (f >>> arr (,()))
--           (Local l (
--               arr distr_unwrap >>>
--               ((c >>> arr wrap) *** arr swap) >>>
--               arr unassoc >>>
--               first (arr g) >>>
--               arr (swap >>> unassoc) >>>
--               first (d >>> arr wrap)
--           )) $
--         lmap (fst >>> unwrap) $ -- But not this? [2]
--         k

optimize' (Comp f (Comm (l :: Proxy l) (l' :: Proxy l'))
          (Comp g (Comm (m :: Proxy m) (m' :: Proxy m'))
           k))
  | isJust (eqT @l @m) && isJust (eqT @l' @m') =
    case (eqT @l @m, eqT @l' @m') of
      (Just Refl, Just Refl) ->
        (,True) $
        fst . optimize' $
        Comp
          (
            f >>>
            (\(a, c) -> ((setup_err, c), (a, c))) >>>
            first g >>>
            (\((a4, _), (a2, c)) -> ((a2, a4), c))
          )
          (Local l $ arr $ uncurry factor_loc) $
        Comp id (Comm l l') $
        Comp (,()) (Local l' $ arr $ \(unwrap, (p, c)) ->
                       let (a2, a4) = unwrap p
                           (_, c1) = g (wrap a2, c)
                       in
                         (wrap a4, c1)) $
        lmap (fst >>> unwrap) $ -- Why can I do this? [1]
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

