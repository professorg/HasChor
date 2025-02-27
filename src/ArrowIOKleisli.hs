module ArrowIOKleisli where

import Control.Monad.IO.Class
import Control.Arrow.ArrowIO
import Control.Arrow (Kleisli (Kleisli))

instance MonadIO m => ArrowIO (Kleisli m) where
  arrIO prog = Kleisli $ liftIO . prog

