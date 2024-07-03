{-# LANGUAGE ExplicitNamespaces #-}

-- | This module defines the interface to HasChor. The client of the library is
-- highly recommended to only use constructs exported by this module.
module ChoreographyArrow (
  -- * Locations and Located Values
  LocTm,
  LocTy,
  type (@),
  mkLoc,

  -- * The Choreo monad
  Choreo,
  -- ** Choreo operations
  locally,
  (~>),
  (~~>),
--   cond,
--   cond',

  -- * Message transport backends
  -- ** The HTTP backend
--   Host,
--   Port,
--  HttpConfig,
--   mkHttpConfig,

  -- * Running choreographies
  runChoreo,
  runChoreography
  ) where

import ChoreographyArrow.Location
import ChoreographyArrow.Choreo
import ChoreographyArrow.Network
--import ChoreographyArrow.Network.Http
import ChoreographyArrow.Network.Local
import Control.Monad.IO.Class
import Data.Proxy
import Control.Arrow.ArrowIO (ArrowIO)
import Data.Profunctor (Profunctor)

--TODO
-- | Run a choreography with a message transport backend.
runChoreography :: (Backend config, ArrowIO ar, Profunctor ar) => config -> Choreo ar b a -> LocTm -> ar b a
runChoreography cfg choreo l = runNetwork cfg l (epp choreo l)
