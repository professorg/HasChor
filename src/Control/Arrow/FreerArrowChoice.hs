{-# LANGUAGE LambdaCase #-}
module Control.Arrow.FreerArrowChoice where

import qualified Data.Bifunctor as B (first)
import Control.Category
import Control.Arrow
import Data.Kind
import Data.Profunctor
import Prelude hiding (id, (.))

-- |- Freer arrow choice. Inspired by:
-- [https://www.reddit.com/r/haskell/comments/p7grsq/comment/h9k2anl/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button]
data FreerArrowChoice e x y where
  Hom :: (x -> y) -> FreerArrowChoice e x y
  Comp :: (x -> Either (a, c) w) -> (Either (b, c) w -> y) ->
    e a b -> FreerArrowChoice e y z -> FreerArrowChoice e x z

-- |- This is called overCount because with FreerArrowChoice, there is no
-- guarantee that theeffect will happen. Instead, the static analyzer gives us
-- the worst case count---how many times effect would appear when all effects
-- happen. (It is not clear to me how to define an underCount---this is possible
-- with selective functor but I don't see how with FreerArrowChoice.)
overCount :: FreerArrowChoice e x y -> Int
overCount (Hom _) = 0
overCount (Comp _ _ _ y) = 1 + overCount y

embed :: e x y -> FreerArrowChoice e x y
embed f = Comp (\x -> Left (x, ()))
               (\case
                   Left (b, _) -> b
                   Right b -> b) f id

instance Arrow (FreerArrowChoice e) where
  arr :: (x -> y) -> FreerArrowChoice e x y
  arr = Hom

  first :: FreerArrowChoice e x y -> FreerArrowChoice e (x, z) (y, z)
  first = first'

instance ArrowChoice (FreerArrowChoice e) where
  left = left'

instance Profunctor (FreerArrowChoice e) where
  dimap f g (Hom h) = Hom (g . h . f)
  dimap f g (Comp f' g' x y) = Comp (f' . f) id x (dimap g' g y)

instance Strong (FreerArrowChoice e) where
  first' (Hom f) = Hom $ B.first f
  first' (Comp f g a b) = Comp (\(x, c) ->
                                  case f x of
                                    Left (x', z) -> Left (x', (z, c))
                                    Right x' -> Right (x', c))
                               (\case
                                   Left (y, (z, x)) -> (g (Left (y, z)), x)
                                   Right (y, z) -> (g (Right y), z))
                          a (first' b)

instance Choice (FreerArrowChoice e) where
  left' (Hom f) = Hom $ \case
    Left x -> Left $ f x
    Right y -> Right y
  left' (Comp f g a b) = Comp (\case
                                  Left x -> case f x of
                                    Left (x', z) -> Left (x', z)
                                    Right w -> Right (Left w)
                                  Right y -> Right (Right y))
                              (\case
                                  Left (y, z) -> Left $ g (Left (y, z))
                                  Right (Left w) -> Left $ g (Right w)
                                  Right (Right y) -> Right y) a (left' b) 

instance Category (FreerArrowChoice e) where
  id = Hom id

  f . (Hom g) = dimap g id f
  f . (Comp f' g' x y) = Comp f' g' x (f . y)


data StateEff :: Type -> Type -> Type -> Type where
  Get :: StateEff s () s
  Put :: StateEff s s ()

get :: FreerArrowChoice (StateEff s) () s
get = embed Get

put :: FreerArrowChoice (StateEff s) s ()
put = embed Put

echo :: FreerArrowChoice (StateEff s) () ()
echo = get >>> put

echo2 :: FreerArrowChoice (StateEff s) () ((), ())
echo2 = get >>> put &&& put

echoWithIf :: FreerArrowChoice (StateEff s) Bool (Either ((), ()) ())
echoWithIf = lmap (\case
                 True -> Left ()
                 False -> Right ()) (left echo2) 

type x ~> y = forall a b. x a b -> y a b

interp :: (Choice arr, Arrow arr) =>
  (e ~> arr) -> FreerArrowChoice e x y -> arr x y
interp _       (Hom f) = arr f
interp handler (Comp f g x y) = dimap f g (left' (first (handler x))) >>>
                                        interp handler y
