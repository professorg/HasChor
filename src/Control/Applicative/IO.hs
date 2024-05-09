module Control.Applicative.IO where

import Control.Concurrent.Chan
import GHC.IO (unsafePerformIO)

ofIOAction :: (a -> IO b) -> IO (a -> b)
ofIOAction f = pure $! unsafePerformIO . f

writeChanA :: Chan a -> IO (a -> ())
writeChanA c =
  let writeChan' = writeChan c in
  ofIOAction writeChan'

putStrLnA :: IO (String -> ())
putStrLnA = ofIOAction putStrLn

(|*>) :: Applicative f => f a -> f (a -> b) -> f b
(|*>) = flip (<*>)

infixr 4 |*>

