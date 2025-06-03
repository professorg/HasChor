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

pack :: KnownSymbol l => a @ l -> b @ l -> (a, b) @ l
pack x y = unLoc $ (,) <$> AtLoc x <*> AtLoc y

unpack :: KnownSymbol l => (a, b) @ l -> (a @ l, b @ l)
unpack p = (unLoc $ fst <$> AtLoc p, unLoc $ snd <$> AtLoc p)

mergeLoc :: KnownSymbol l => a @ l -> a @ l -> a @ l
mergeLoc (Wrap x) _ = Wrap x
mergeLoc _ (Wrap x) = Wrap x
mergeLoc Empty Empty = Empty

choreo :: (ArrowIO ar, Strong ar) => Choreo ar () (Integer @ "alice", Integer @ "alice")
choreo =
  discard >>>
  alice `locally0` arr (const (5 :: Integer)) >>>
  (alice ~> bob) >>>
  bob `locally0` arr (const (6 :: Integer)) &&& id >>>
  arr fst &&& (arr (uncurry pack) >>> bob `locally` arr (uncurry (+))) >>>
  (bob ~> alice) *** (bob ~> alice) >>>
  id &&& (arr (uncurry pack) >>> alice `locally` arrIO print) >>>
  arr fst

choreo_IO :: Choreo (Kleisli IO) () (Integer @ "alice", Integer @ "alice")
choreo_IO = choreo

choreo_epp :: (ArrowIO ar, Strong ar) => LocTm -> Network ar () (Integer @ "alice", Integer @ "alice")
choreo_epp = epp choreo

choreo_epp_IO :: LocTm -> Network (Kleisli IO) () (Integer @ "alice", Integer @ "alice")
choreo_epp_IO = choreo_epp

choreo_run_IO :: Kleisli IO () (Integer @ "alice", Integer @ "alice")
choreo_run_IO = runChoreo choreo

setup_err :: a
setup_err = error "Dummy input matched"

setup_err_loc :: a @ l
setup_err_loc = Empty

-- isAt :: (KnownSymbol l, KnownSymbol l') => a @ l' -> Proxy l -> Bool
-- isAt = isJust (eqT @l @l')

optimize' :: Arrow ar => Choreo ar a b -> (Choreo ar a b, Bool)

optimize' (Hom f) = (Hom f, False)

--TODO: This doesn't work because d could depend on c.
-- g either routes the output of c to the first or second part
-- of the pair it outputs. If it outputs to the first part, we
-- get some (a @ l) which can be passed to d without issue. If
-- it routes it to the second part, it's in some type c that we
-- don't know anything about. It would be great to express this
-- routing in the types, but I don't know if that's reasonable
-- (it borders on using dependent types in a way that might be
-- unmanageable)
--
-- I wonder if it makes sense to choose a different represent-
-- ation for the for the context which would make it more
-- obvious where things end up based on the type of the envir-
-- onment itself
--
--The cases visualized (and the solution that I could use if
--I could distinguish them):
--
-- I will highlight the input to d with double lines
--
-- 1) c and d are independent
--
--           _____f_____         ___c___         _____g_____        ___d___
--          |           |-------|_______|-------|         /=|======|_______|-----
--      ----|           |                       | //=====// |
--          |___________|=======================|=/_________|--------------------
--
--   In this case we can run g early with a dummy first half of the input to
--   get the input to d ahead of time
--
-- 2) d depends on c
--
--           _____f_____         ___c___         _____g_____        ___d___
--          |           |-------|_______|=======|===========|======|_______|-----
--      ----|           |                       |           |
--          |___________|-----------------------|___________|--------------------
--
--   In this case we need to run c before running d, so we might as well have
--   the full input to g. However, we can provide a dummy input in the second
--   half if we want.
--
-- 2) d depends on c and unrelated output from f
--
--           _____f_____         ___c___         _____g_____        ___d___
--          |           |-------|_______|=======|===========|======|_______|-----
--      ----|           |                       | //        |
--          |___________|=======================|=/_________|--------------------
--
--   In this case there is basically nothing we can do. We need both inputs
--   to g to be able to run d.
--
-- Something similar arises for finding what the second output of g depends on.
-- If it depends on c, we can't take it out of the Local computation without
-- unwrapping shenanigans.
--
--
optimize' (Comp f (Local (l :: Proxy l) c) (Comp g (Local (l' :: Proxy l') d) k))
  | isJust (eqT @l @l') =
    case (eqT @l @l') of
      Just Refl ->
        (,True) $
        optimize $
        Comp (
          f >>>
          (\(b2, c) -> (pack b2 (fst $ g (setup_err_loc, c)), c))
        ) (Local l (
          first c >>>
          id &&& arr (\(a2, b4) -> g (wrap a2, setup_err)) >>>
          arr (\((a2, b41), (b42, _)) -> (a2, unwrap $ mergeLoc (wrap b41) b42)) >>>
          second d
        )) $
        lmap (
          first unpack >>>
          (\((a2, a4), c) -> (g (a2, c), a4)) >>>
          (\((_, c), a4) -> (a4, c))
        ) $
        k


-- ((a @ l, b @ l'), c @ l)
-- ((a, c) @ l, b @ l')

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
          (\(a2, c) -> (pack a2 (fst $ g (setup_err_loc, c)), c))
        ) (Comm l l') $
        lmap (
          first unpack >>>
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

count_send_recv :: Network ar a b -> (Integer, Integer)
count_send_recv (Comp _ e k) =
  let (s, r) = count_send_recv k in
    case e of
      Send _ -> (s + 1, r)
      Recv _ -> (s, r + 1)
      _ -> (s, r)
count_send_recv (Hom _) = (0, 0)

main' :: Choreo (Kleisli IO) () (Integer @ "alice", Integer @ "alice") -> IO ()
main' c = do
  config <- mkLocalConfig locs
  mapConcurrently_ (\l -> runChoreography config c l ()) locs
  return ()
  where
    locs = ["alice", "bob"]

main :: IO ()
main = main' choreo

