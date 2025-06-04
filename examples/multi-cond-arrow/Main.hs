
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Main where

import ChoreographyArrowChoice (runChoreography)
import ChoreographyArrowChoice.Choreo
import ChoreographyArrowChoice.Location
import ChoreographyArrowChoice.Network
import Data.Proxy
import GHC.TypeLits
import Control.Arrow
import Data.Profunctor
import Control.Arrow.Freer.FreerChoiceArrow
import Prelude hiding (id, (.))
import Control.Category

alice :: Proxy "alice"
alice = Proxy

bob :: Proxy "bob"
bob = Proxy

charlie :: Proxy "charlie"
charlie = Proxy

eve :: Proxy "eve"
eve = Proxy

degenerate :: Choreo (Kleisli IO) () ()
degenerate =
  alice `locally0` (Kleisli . const) getLine >>>
  cond alice id >>>
  -- wrap as bob
  arr wrap >>>
  -- do something with the value
  bob `locally` Kleisli putStrLn >>>
  discard

putStrLnRet :: Show a => Kleisli IO a a
putStrLnRet = Kleisli \x -> do
  putStrLn $ show x
  pure x

choreo :: Choreo (Kleisli IO) () ()
choreo =
  alice `locally0` (Kleisli . const) getLine >>>
  cond' alice (arr \s -> if length s > 0 then Left s else Right ()) (
    (+++)
      ((&&&)
        (arr wrap >>> bob `locally` putStrLnRet >>>
         bob `locally0` (Kleisli . const) getLine)
        (arr wrap >>> charlie `locally` putStrLnRet >>>
         charlie `locally0` (Kleisli . const) getLine)
      )
      id
  ) >>>
  discard -- TODO: chain another cond

main :: IO ()
main = pure ()
