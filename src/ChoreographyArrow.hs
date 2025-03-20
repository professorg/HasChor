{-# LANGUAGE ExplicitNamespaces #-}

-- | This module defines the interface to HasChor. The client of the library is
-- highly recommended to only use constructs exported by this module.
module ChoreographyArrow (
  -- * Locations and Located Values
  LocTm,
  LocTy,
  type (@),
  mkLoc,
  AtLoc,
  unLoc,

  -- * The Choreo monad
  Choreo,
  -- ** Choreo operations
  discard,
  locally,
  locally0,
  (~>),
  (~~>),
--   cond,
--   cond',

  -- * Message transport backends
  -- ** The HTTP backend
  Host,
  Port,
  HttpConfig,
  mkHttpConfig,

  -- * Running choreographies
  runChoreo,
  runChoreography
  ) where

import ChoreographyArrow.Location
import ChoreographyArrow.Choreo
import ChoreographyArrow.Network
import ChoreographyArrow.Network.Http
import ChoreographyArrow.Network.Local
import Control.Monad.IO.Class
import Data.Proxy
import Control.Arrow.ArrowIO (ArrowIO)
import Data.Profunctor (Profunctor)
import Control.Arrow (Kleisli)

--TODO
-- | Run a choreography with a message transport backend.
runChoreography :: (Backend config, MonadIO m) => config -> Choreo (Kleisli m) b a -> LocTm -> b -> m a
runChoreography cfg choreo l = runNetwork cfg l (epp choreo l)
