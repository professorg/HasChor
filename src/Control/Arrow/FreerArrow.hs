module Control.Arrow.FreerArrow where

import qualified Data.Bifunctor as B (first)
import Control.Category
import Control.Arrow
import Data.Kind
import Data.Profunctor
import Prelude hiding (id, (.))

data FreerArrow e x y where
  Hom :: (x -> y) -> FreerArrow e x y
  Comp :: (x -> (a, c)) -> ((b, c) -> y) ->
    e a b -> FreerArrow e y z -> FreerArrow e x z

count :: FreerArrow e x y -> Int
count (Hom _) = 0
count (Comp _ _ _ y) = 1 + count y

{--
data ReifiedArrow e x y where
  HomR :: (x -> y) -> ReifiedArrow e x y
  Embed :: e x y -> ReifiedArrow e x y
  CompR :: (x -> (a, c)) -> ((b, c) -> y) ->
    ReifiedArrow e a b -> ReifiedArrow e y z -> ReifiedArrow e x z
--}
    
embed :: e x y -> FreerArrow e x y
embed f = Comp (,()) fst f id

instance Arrow (FreerArrow e) where
  arr :: (x -> y) -> FreerArrow e x y
  arr = Hom

  first :: FreerArrow e x y -> FreerArrow e (x, z) (y, z)
  first = first'

instance Profunctor (FreerArrow e) where
  dimap f g (Hom h) = Hom (g . h . f)
  dimap f g (Comp f' g' x y) = Comp (f' . f) id x (dimap g' g y)

instance Strong (FreerArrow e) where
  first' (Hom f) = Hom $ B.first f
  first' (Comp f g a b) = Comp (\(x, c) ->
                                  case f x of
                                    (x', z) -> (x', (z, c)))
                               (\(y, (z, x)) -> (g (y, z), x))
                          a (first' b)

instance Category (FreerArrow e) where
  id = Hom id

  f . (Hom g) = dimap g id f
  f . (Comp f' g' x y) = Comp f' g' x (f . y)


data StateEff :: Type -> Type -> Type -> Type where
  Get :: StateEff s () s
  Put :: StateEff s s ()

get :: FreerArrow (StateEff s) () s
get = embed Get

put :: FreerArrow (StateEff s) s ()
put = embed Put

echo :: FreerArrow (StateEff s) () ()
echo = put . get

echo2 :: FreerArrow (StateEff s) () ((), ())
echo2 = dimap id (\x -> (x, x)) get >>> first put >>> second put  

type x ~> y = forall a b. x a b -> y a b

interp :: (Profunctor arr, Arrow arr) =>
  (e ~> arr) -> FreerArrow e x y -> arr x y
interp _       (Hom f) = arr f
interp handler (Comp f g x y) = dimap f g (first (handler x)) >>>
                                        interp handler y
