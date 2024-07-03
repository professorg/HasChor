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
import ChoreographyArrow.Network.Local (mkLocalConfig)
import ChoreographyArrow (runChoreography)
import ChoreographyArrow.Network (Backend)

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
      arrIO0 (putStrLn "enter to start key exchange...") >>>
      arrIO0 getLine >>>
      discard
    )

bobWait :: ArrowIO ar => Choreo ar () (() @ "bob")
bobWait =
  bob `locally` (
    arrIO0 (putStrLn "waiting for alice to initiate key exchange")
    )

genSecret' :: ArrowIO ar => ar () Integer
genSecret' = arrIO0 (randomRIO (200, 1000 :: Integer))

genSecret :: ArrowIO ar => ar b Integer
genSecret = discard >>> genSecret'

diffieHellman :: (ArrowIO ar, Strong ar) => Choreo ar () (Integer @ "alice", Integer @ "bob")
diffieHellman = proc () -> do
  -- wait for alice to initiate the process
  aliceWait -< ()
  bobWait -< ()

  pa <- alice `locally` (
      arrIO0 (randomRIO (200, 1000 :: Int)) >>>
      arr (primeNums !! )
    ) -< ()

  pb <- (alice ~> bob) -< pa

  ga <- (alice `locally` (
      arr (\(unwrap, pa) -> (10, unwrap pa)) >>>
      arrIO randomRIO
    )) -< pa

  gb <-  (alice ~> bob) -< ga

  a <- (alice `locally` genSecret) -< ()

  b <- (bob `locally` genSecret) -< ()

  a' <- (alice `locally` (
      arr (\(unwrap, (ga, a, pa)) -> unwrap ga ^ unwrap a `mod` unwrap pa)
    )) -< (ga, a, pa)

  b' <- (bob `locally` (
      arr (\(unwrap, (gb, b, pb)) -> unwrap gb ^ unwrap b `mod` unwrap pb)
    )) -< (gb, b, pb)

  a'' <-  (alice ~> bob) -< a'

  b'' <- (bob ~> alice) -< b'

  s1 <- (alice `locally` (proc (unwrap, (b'', a, pa)) -> do
      s <- arr (\(unwrap, (b'', a, pa)) -> unwrap b'' ^ unwrap a `mod` unwrap pa) -< (unwrap, (b'', a, pa))
      (
          arr (\s -> "alice's shared key: " ++ show s) >>>
          arrIO putStrLn
        ) -< s
      returnA -< s
    )) -< (b'', a, pa)

  s2 <- (bob `locally` (proc (unwrap, (b'', a, pa)) -> do
      s <- arr (\(unwrap, (a'', b, pb)) -> unwrap a'' ^ unwrap b `mod` unwrap pb) -< (unwrap, (b'', a, pa))
      (
          arr (\s -> "bob's shared key: " ++ show s) >>>
          arrIO putStrLn
        ) -< s
      returnA -< s
    )) -< (a'', b, pb)

  returnA -< (s1, s2)

instance ArrowIO (Kleisli IO) where
  arrIO f = Kleisli f

-- Kleisli IO a b
-- a -> IO b

main' :: Backend config => config -> LocTm -> Kleisli IO () (Integer @ "alice", Integer @ "bob")
main' config l = runChoreography config diffieHellman l

--TODO
main :: IO ()
main = do
  [loc] <- getArgs
  config <- mkLocalConfig ["alice", "bob"]
  x <- case loc of
    "alice" -> runKleisli (main' config "alice") ()
    "bob" -> runKleisli (main' config "bob") ()
  return ()

