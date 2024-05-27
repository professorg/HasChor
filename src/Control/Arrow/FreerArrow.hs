module Control.Arrow.FreerArrow where

import qualified Data.Bifunctor as B (first)
import Control.Category
import Control.Arrow
import Data.Kind
import Data.Profunctor
import Prelude hiding (id, (.))

-- |- Freer arrows. This is essentially free arrows (Notions of computation as
-- monoids, Rivas & Jaskelioff, JFP) inlined with free strong profunctors.
data FreerArrow e x y where
  Hom :: (x -> y) -> FreerArrow e x y
  Comp :: (x -> (a, c)) -> ((b, c) -> y) ->
    e a b -> FreerArrow e y z -> FreerArrow e x z

-- A function that counts the number of effects in a freer arrow. This
-- illustrates that some static analysis can be performed on freer arrows. Even
-- if the effect can be stateful, we do not need to provide an initial state.
count :: FreerArrow e x y -> Int
count (Hom _) = 0
count (Comp _ _ _ y) = 1 + count y

-- The following is what I call a reified arrow. It is free if we define an
-- equality that satisifies arrow laws and profunctor laws.
{--
data ReifiedArrow e x y where
  HomR :: (x -> y) -> ReifiedArrow e x y
  Embed :: e x y -> ReifiedArrow e x y
  CompR :: (x -> (a, c)) -> ((b, c) -> y) ->
    ReifiedArrow e a b -> ReifiedArrow e y z -> ReifiedArrow e x z
--}

-- |- Embed an effect in freer arrows.                        
embed :: e x y -> FreerArrow e x y
embed f = Comp (,()) fst f id

-- |- Freer arrows are arrows.
instance Arrow (FreerArrow e) where
  arr :: (x -> y) -> FreerArrow e x y
  arr = Hom

  first :: FreerArrow e x y -> FreerArrow e (x, z) (y, z)
  first = first'

-- |- Freer arrows are profunctors.
instance Profunctor (FreerArrow e) where
  dimap f g (Hom h) = Hom (g . h . f)
  dimap f g (Comp f' g' x y) = Comp (f' . f) g' x (dimap id g y)

-- |- Freer arrows are strong profunctors.
instance Strong (FreerArrow e) where
  first' (Hom f) = Hom $ B.first f
  first' (Comp f g a b) = Comp (\(x, c) ->
                                  case f x of
                                    (x', z) -> (x', (z, c)))
                               (\(y, (z, x)) -> (g (y, z), x))
                          a (first' b)

-- |- Freer arrows are categories.
instance Category (FreerArrow e) where
  id = Hom id

  f . (Hom g) = dimap g id f
  f . (Comp f' g' x y) = Comp f' g' x (f . y)


-- |- An ADT for stateful effect.
data StateEff :: Type -> Type -> Type -> Type where
  Get :: StateEff s () s
  Put :: StateEff s s ()

-- |- A "smart constructor" for get effect.
get :: FreerArrow (StateEff s) () s
get = embed Get

-- |- A "smart constructor" for put effect.
put :: FreerArrow (StateEff s) s ()
put = embed Put

-- |- A program that reads from the state and writes back the state.
echo :: FreerArrow (StateEff s) () ()
echo = get >>> put

-- |- Echo but with data sharing. You get once but put twice.
echo2 :: FreerArrow (StateEff s) () ((), ())
-- (>>>) is from the Category typeclass. Every arrow is a category. (&&&) is an
-- arrow combinator.
echo2 = get >>> put &&& put

-- |- The type for effect handlers.
type x ~> y = forall a b. x a b -> y a b

-- |- Freer arrows can be interpreted into any arrows, as long as we can provide
-- an effect handler.
interp :: (Profunctor arr, Arrow arr) =>
  (e ~> arr) -> FreerArrow e x y -> arr x y
interp _       (Hom f) = arr f
interp handler (Comp f g x y) = dimap f g (first (handler x)) >>>
                                        interp handler y
