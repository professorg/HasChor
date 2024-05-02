{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Choreography (mkHttpConfig, runChoreography)
import ChoreographyA.Choreo
import Choreography.Location
import Data.Proxy
import Data.Time
import System.Environment
import System.Random
import Control.Applicative.IO ((|*>), putStrLnA, ofIOAction)
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
  let a_init = alice `locally` (const <$> (
          putStrLn "enter to start key exchange..." *>
          getLine
        )) in
  let b_wait = bob `locally` (const <$> 
          putStrLn "waiting for alice to initiate key exchange"
        ) in
  let pa = alice `locally` (const <$> (
          (primeNums !!) <$>
          randomRIO (200, 1000 :: Int)
        )) in
  let pb = (pa |*> (alice ~> bob)) in
  let ga = 
          alice `locally` (liftA2 _ _ _)
          --   (pure (\x f unwrap -> f (10, unwrap x))))
          -- <*> pa
          -- <*> randomRIOA in
          in
  a_init *>
  b_wait *>
  _
  -- bob `locally` (pure $ \unwrap -> do
  --   putStrLn "waiting for alice to initiate key exchange")
  -- |*>
  -- alice `locally` (pure $ \unwrap -> do
  --   x <- randomRIO (200, 1000 :: Int)
  --   return $ primeNums !! x)
  -- |*>
  -- (alice, Proxy) ~> bob
  -- |*> do


  -- -- alice picks p and g and sends them to bob
  -- pb <- (alice, pa) ~> bob
  -- ga <- alice `locally` \unwrap -> do randomRIO (10, unwrap pa)
  -- gb <- (alice, ga) ~> bob

  -- -- alice and bob select secrets
  -- a <- alice `locally` \unwrap -> do randomRIO (200, 1000 :: Integer)
  -- b <- bob `locally` \unwrap -> do randomRIO (200, 1000 :: Integer)

  -- -- alice and bob computes numbers that they exchange
  -- a' <- alice `locally` \unwrap -> do return $ unwrap ga ^ unwrap a `mod` unwrap pa
  -- b' <- bob `locally` \unwrap -> do return $ unwrap gb ^ unwrap b `mod` unwrap pb

  -- -- exchange numbers
  -- a'' <- (alice, a') ~> bob
  -- b'' <- (bob, b') ~> alice

  -- -- compute shared key
  -- s1 <-
  --   alice `locally` \unwrap ->
  --     let s = unwrap b'' ^ unwrap a `mod` unwrap pa
  --      in do
  --           putStrLn ("alice's shared key: " ++ show s)
  --           return s
  -- s2 <-
  --   bob `locally` \unwrap ->
  --     let s = unwrap a'' ^ unwrap b `mod` unwrap pb
  --      in do
  --           putStrLn ("bob's shared key: " ++ show s)
  --           return s
  -- return (s1, s2)

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
