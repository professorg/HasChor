{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Choreography (mkHttpConfig, runChoreography)
import ChoreographyA.Choreo
import ChoreographyA.Location
import Data.Proxy
import Data.Time
import System.Environment
import System.Random
import Control.Applicative.IO ((|*>), putStrLnA, ofIOAction)
import Control.Lens.Lens
import Data.Functor

-- helper functions around prime number
-- https://nulldereference.wordpress.com/2012/02/04/generating-prime-numbers-with-haskell/
divisors :: Integer -> [Integer]
divisors 1 = [1]
divisors x = 1 : [y | y <- [2 .. (x `div` 2)], x `mod` y == 0] ++ [x]

isPrime :: Integer -> Bool
isPrime x = divisors x == [1, x]

primeNums :: [Integer]
primeNums = [x | x <- [2 ..], isPrime x]

randomRIOA :: Random a => IO ((a, a) -> a)
randomRIOA = ofIOAction randomRIO

-- set up proxies
alice :: Proxy "alice"
alice = Proxy

bob :: Proxy "bob"
bob = Proxy

diffieHellman :: ChoreoA IO (Integer @ "alice", Integer @ "bob")
diffieHellman =
  -- wait for alice to initiate the process
  -- type Unwrap l = forall a. a @ l -> a
  let a_init = alice `locally` \_ ->
          (putStrLnA ?? "enter to start key exchange...") *> getLine in
  let b_wait = bob `locally` \_ ->
          putStrLnA ?? "waiting for alice to initiate key exchange" in
    
  let pa = alice `locally` \_ ->
          (primeNums !!) <$>
          (randomRIOA ?? (200, 1000 :: Int)) in
  let pb = (pa |*> (alice ~> bob)) in
  let ga = liftA2 compLL
              (alice `locally` \unwrap -> (\f x -> f (10, unwrap x)) <$> randomRIOA)
              pa in
  let gb = (ga |*> (alice ~> bob)) in
    
  -- alice and bob select secrets
  let a  = alice `locally` \_ -> randomRIOA ?? (200, 1000 :: Integer) in 
  let b  = bob   `locally` \_ -> randomRIOA ?? (200, 1000 :: Integer) in

  let a' = compLL3 <$> (alice `locally` (\unwrap ->
                                           pure (\a b c -> unwrap a ^ unwrap b `mod` unwrap c)))
           <*> ga <*> a <*> pa in
  let b' = compLL3 <$> (bob   `locally` (\unwrap ->
                                           pure (\a b c -> unwrap a ^ unwrap b `mod` unwrap c)))
           <*> gb <*> b <*> pb in

  let a'' = (a' |*> (alice ~> bob)) in 
  let b'' = (b' |*> (bob ~> alice)) in

  let s1 = compLL3 <$> (alice `locally` (\unwrap ->
                                           (\f a b c ->
                                              let s = unwrap a ^ unwrap b `mod` unwrap c in
                                                f ("alice's shared key: " ++ show s) `seq` s) <$> putStrLnA))
           <*> b'' <*> a <*> pa in
  let s2 = compLL3 <$> (bob   `locally` (\unwrap ->
                                           (\f a b c ->
                                              let s = unwrap a ^ unwrap b `mod` unwrap c in
                                                f ("bob's shared key: " ++ show s) `seq` s) <$> putStrLnA))
           <*> a'' <*> b <*> pb in

  a_init *> b_wait *> ((,) <$> s1 <*> s2)

main :: IO ()
main = do
  [loc] <- getArgs
  x <- case loc of
    "alice" -> runChoreography config diffieHellman "alice"
    "bob" -> runChoreography config diffieHellman "bob"
  return ()
  where
    config =
      mkHttpConfig
        [ ("alice", ("localhost", 5000)),
          ("bob", ("localhost", 5001))
        ]
