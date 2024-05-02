{-# LANGUAGE ExplicitNamespaces #-}

-- | This module defines the interface to HasChor. The client of the library is
-- highly recommended to only use constructs exported by this module.
module ChoreographyA (
  -- * Locations and Located Values
  -- * The Choreo monad
  ChoreoA,
  -- ** Choreo operations
  locally,
  (~>),
  (~~>),
  cond,
  cond',

  -- * Message transport backends
  -- ** The HTTP backend
--   Host,
--   Port,
--   HttpConfig,
--    mkHttpConfig,

  -- * Running choreographies
  runChoreoA,
  runChoreography
  ) where

import Control.Monad.IO.Class
import ChoreographyA.Location
import ChoreographyA.Choreo
import ChoreographyA.Network
-- import ChoreographyA.Network.Http
import ChoreographyA.Network.Local
import Data.Proxy

-- | Run a choreography with a message transport backend.
runChoreography :: (Backend config, MonadIO f) => config -> ChoreoA f a -> LocTm -> f a
runChoreography cfg choreo l = runNetwork cfg l (epp choreo l)
