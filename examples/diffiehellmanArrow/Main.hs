{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main where

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
import Control.Arrow.Freer.FreerChoiceArrow
import ChoreographyArrow (mkHttpConfig, runChoreography)
import ChoreographyArrow.Network (Backend)
import GHC.TypeLits

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

aliceWait :: ArrowIO ar => Choreo ar () ()
aliceWait =
  alice `locally0` (
      arrIO0 (putStrLn "enter to start key exchange...") >>>
      arrIO0 getLine
    ) >>>
  discard

bobWait :: ArrowIO ar => Choreo ar () ()
bobWait =
  bob `locally0` (
    arrIO0 (putStrLn "waiting for alice to initiate key exchange")
    ) >>>
  discard

genSecret' :: ArrowIO ar => ar () Integer
genSecret' = arrIO0 (randomRIO (200, 1000 :: Integer))

genSecret :: ArrowIO ar => ar b Integer
genSecret = discard >>> genSecret'

atAlice :: a @ alice -> AtLoc alice a
atAlice = AtLoc

atBob :: a @ bob -> AtLoc bob a
atBob = AtLoc

pack :: KnownSymbol l => a @ l -> b @ l -> (a, b) @ l
pack x y = unLoc $ (,) <$> AtLoc x <*> AtLoc y

unpack :: KnownSymbol l => (a, b) @ l -> (a @ l, b @ l)
unpack p = (unLoc $ fst <$> AtLoc p, unLoc $ snd <$> AtLoc p)

pack3 :: KnownSymbol l => a @ l -> b @ l -> c @ l -> (a, b, c) @ l
pack3 x y z = unLoc $ (,,) <$> AtLoc x <*> AtLoc y <*> AtLoc z

unpack3 :: KnownSymbol l => (a, b, c) @ l -> (a @ l, b @ l, c @ l)
unpack3 p = (unLoc $ (\(x, _, _) -> x) <$> AtLoc p
            ,unLoc $ (\(_, x, _) -> x) <$> AtLoc p
            ,unLoc $ (\(_, _, x) -> x) <$> AtLoc p)

diffieHellman :: (ArrowIO ar, Strong ar) => Choreo ar () (Integer @ "alice", Integer @ "bob")
diffieHellman = proc () -> do
  -- wait for alice to initiate the process
  aliceWait -< ()
  bobWait -< ()

  pa <- alice `locally0` (
      arrIO0 (randomRIO (200, 1000 :: Int)) >>>
      arr (primeNums !! )
    ) -< ()

  pb <- (alice ~> bob) -< pa

  ga <- (alice `locally` (
      arr (10,) >>>
      arrIO randomRIO
    )) -< pa

  gb <-  (alice ~> bob) -< ga

  a <- (alice `locally` genSecret) -< wrap ()

  b <- (bob `locally` genSecret) -< wrap ()

  a' <- alice `locally`
    arr (\(ga, a, pa) -> ga ^ a `mod` pa
    ) -< pack3 ga a pa

  b' <- bob `locally`
    arr (\(gb, b, pb) -> gb ^ b `mod` pb
    ) -< pack3 gb b pb

  a'' <-  (alice ~> bob) -< a'

  b'' <- (bob ~> alice) -< b'

  s1 <- (alice `locally` (proc (b'', a, pa) -> do
      s <- arr (\(b'', a, pa) -> b'' ^ a `mod` pa) -< (b'', a, pa)
      (
          arr (\s -> "alice's shared key: " ++ show s) >>>
          arrIO putStrLn
        ) -< s
      returnA -< s
    )) -< pack3 b'' a pa

  s2 <- (bob `locally` (proc (a'', b, pb) -> do
      s <- arr (\(a'', b, pb) -> a'' ^ b `mod` pb) -< (a'', b, pb)
      (
          arr (\s -> "bob's shared key: " ++ show s) >>>
          arrIO putStrLn
        ) -< s
      returnA -< s
    )) -< pack3 a'' b pb

  returnA -< (s1, s2)

-- Kleisli IO a b
-- a -> IO b

main :: IO ()
main = do
  [loc] <- getArgs
  x <- case loc of
    "alice" -> runChoreography config diffieHellman "alice" ()
    "bob" -> runChoreography config diffieHellman "bob" ()
  return ()
  where
    config =
      mkHttpConfig
        [ ("alice", ("localhost", 5000)),
          ("bob", ("localhost", 5001))
        ]

