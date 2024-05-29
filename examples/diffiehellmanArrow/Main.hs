{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main where

--import ChoreographyArrow (mkHttpConfig, runChoreography)
import ChoreographyArrow.Choreo
import ChoreographyArrow.Location
import Data.Proxy
import Data.Time
import System.Environment
import System.Random
import Data.Profunctor (Profunctor, Strong, first', second')
import Control.Arrow
import Control.Arrow.ArrowIO
import Control.Category
import Prelude hiding (id, (.))
import Control.Arrow.FreerArrow

-- helper functions around prime number
-- https://nulldereference.wordpress.com/2012/02/04/generating-prime-numbers-with-haskell/
divisors :: Integer -> [Integer]
divisors 1 = [1]
divisors x = 1 : [y | y <- [2 .. (x `div` 2)], x `mod` y == 0] ++ [x]

isPrime :: Integer -> Bool
isPrime x = divisors x == [1, x]

primeNums :: [Integer]
primeNums = [x | x <- [2 ..], isPrime x]

-- set up proxies
alice :: Proxy "alice"
alice = Proxy

bob :: Proxy "bob"
bob = Proxy

discard :: Arrow ar => ar b ()
discard = arr (const ())

aliceWait :: ArrowIO ar => Choreo ar () (() @ "alice")
aliceWait =
  alice `locally` (
    discard >>>
    arrIO0 (putStrLn "enter to start key exchange...") >>>
    arrIO0 getLine >>>
    discard
    )

bobWait :: ArrowIO ar => Choreo ar () (() @ "bob")
bobWait =
  bob `locally` (
    discard >>>
    arrIO0 (putStrLn "waiting for alice to initiate key exchange")
    )

diffieHellman :: (ArrowIO ar, Strong ar) => Choreo ar () (Integer @ "alice", Integer @ "bob")
diffieHellman = --do
  -- wait for alice to initiate the process
  aliceWait >>> discard >>>
  bobWait >>> discard >>>

  alice `locally` (
    discard >>>
    arrIO0 (randomRIO (200, 1000 :: Int)) >>>
    arr (primeNums !! )
  ) >>> -- pa

  arr (\pa -> (pa, pa)) >>>

  second' (alice ~> bob) >>> -- (pa, pb)

  arr (\(pa, pb) -> (pa, (pa, pb))) >>>

  first' (alice `locally` (
    arr (\(unwrap, pa) -> (10, unwrap pa)) >>>
    arrIO randomRIO
  )) >>> -- (ga, (pa, pb))

  arr (\(ga, (pa, pb)) -> (ga, (ga, pa, pb))) >>>

  first' (alice ~> bob) >>> -- (gb, (ga, pa, pb))

  arr (\(gb, (ga, pa, pb)) -> ((), (ga, gb, pa, pb))) >>>

  first' (alice `locally` (
    discard >>>
    arrIO0 (randomRIO (200, 1000 :: Integer))
  )) >>> -- (a, (ga, gb, pa, pb))

  arr (\(a, (ga, gb, pa, pb)) -> ((), (a, ga, gb, pa, pb))) >>>

  first' (bob `locally` (
    discard >>>
    arrIO0 (randomRIO (200, 1000 :: Integer))
  )) >>> -- (b, (a, ga, gb, pa, pb))

  arr (\(b, (a, ga, gb, pa, pb)) -> ((), (a, b, ga, gb, pa, pb))) >>>


  arr (\((), (a, b, ga, gb, pa, pb)) -> ((ga, a, pa), (a, b, ga, gb, pa, pb))) >>>

  first' (alice `locally` (
    arr (\(unwrap, (ga, a, pa)) -> unwrap ga ^ unwrap a `mod` unwrap pa)
  )) >>> -- (a', (a, b, ga, gb, pa, pb))

  arr (\(a', (a, b, ga, gb, pa, pb)) -> ((), (a', a, b, ga, gb, pa, pb))) >>>

  arr (\((), (a', a, b, ga, gb, pa, pb)) -> ((gb, b, pb), (a', a, b, ga, gb, pa, pb))) >>>

  first' (bob `locally` (
    arr (\(unwrap, (gb, b, pb)) -> unwrap gb ^ unwrap b `mod` unwrap pb)
  )) >>> -- (b', (a', a, b, ga, gb, pa, pb))

  arr (\(b', (a', a, b, ga, gb, pa, pb)) -> ((), (a', b', a, b, ga, gb, pa, pb))) >>>

  arr (\((), (a', b', a, b, ga, gb, pa, pb)) -> (a', (a', b', a, b, ga, gb, pa, pb))) >>>

  first' (alice ~> bob) >>> -- (a'', (a', b', a, b, ga, gb, pa, pb))

  arr (\(a'', (a', b', a, b, ga, gb, pa, pb)) -> ((), (a'', a', b', a, b, ga, gb, pa, pb))) >>>

  arr (\((), (a'', a', b', a, b, ga, gb, pa, pb)) -> (b', (a'', a', b', a, b, ga, gb, pa, pb))) >>>

  first' (bob ~> alice) >>> -- (b'', (a'', a', b', a, b, ga, gb, pa, pb))

  arr (\(b'', (a'', a', b', a, b, ga, gb, pa, pb)) -> ((), (a'', b'', a', b', a, b, ga, gb, pa, pb))) >>>

  arr (\((), (a'', b'', a', b', a, b, ga, gb, pa, pb)) -> ((b'', a, pa), (a'', b'', a', b', a, b, ga, gb, pa, pb))) >>>

  first' (alice `locally` (
    arr (\(unwrap, (b'', a, pa)) -> unwrap b'' ^ unwrap a `mod` unwrap pa) >>>
    arr (\s -> (s, s)) >>>
    second' (
      arr (\s -> "alice's shared key: " ++ show s) >>>
      arrIO putStrLn
    ) >>>
    arr fst
  )) >>> -- (s1, (a'', b'', a', b', a, b, ga, gb, pa, pb))

  arr (\(s1, (a'', b'', a', b', a, b, ga, gb, pa, pb)) -> ((), (s1, a'', b'', a', b', a, b, ga, gb, pa, pb))) >>>

  arr (\((), (s1, a'', b'', a', b', a, b, ga, gb, pa, pb)) -> ((a'', b, pb), (s1, a'', b'', a', b', a, b, ga, gb, pa, pb))) >>>

  first' (bob `locally` (
    arr (\(unwrap, (a'', b, pb)) -> unwrap a'' ^ unwrap b `mod` unwrap pb) >>>
    arr (\s -> (s, s)) >>>
    second' (
      arr (\s -> "bob's shared key: " ++ show s) >>>
      arrIO putStrLn
    ) >>>
    arr fst
  )) >>> -- (s2, (s1, a'', b'', a', b', a, b, ga, gb, pa, pb))

  arr (\(s2, (s1, a'', b'', a', b', a, b, ga, gb, pa, pb)) -> ((), (s1, s2, a'', b'', a', b', a, b, ga, gb, pa, pb))) >>>

  arr (\((), (s1, s2, a'', b'', a', b', a, b, ga, gb, pa, pb)) -> (s1, s2))

--TODO
main :: IO ()
main = pure ()
-- main = do
--   [loc] <- getArgs
--   x <- case loc of
--     "alice" -> runChoreography config diffieHellman "alice"
--     "bob" -> runChoreography config diffieHellman "bob"
--   return ()
--   where
--     config =
--       mkHttpConfig
--         [ ("alice", ("localhost", 5000)),
--           ("bob", ("localhost", 5001))
--         ]
