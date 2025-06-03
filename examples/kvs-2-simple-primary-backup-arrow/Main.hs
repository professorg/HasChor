{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ChoreographyArrowChoice (runChoreography)
import ChoreographyArrowChoice.Choreo
import ChoreographyArrowChoice.Location
import ChoreographyArrowChoice.Network
import Data.Proxy
import Control.Arrow

type Client = "client"

client :: Proxy Client
client = Proxy

primary :: Proxy "primary"
primary = Proxy

backup :: Proxy "backup"
backup = Proxy

type Key = String

type Value = String

data Request where
  Get :: Key -> Request
  Put :: Key -> Value -> Request
  deriving (Show, Read)

data Response
  deriving (Show, Read)

getRequest :: Kleisli ar () Request
getRequest = error "not implemented"

handleRequest :: Kleisli IO Request Response
handleRequest = error "not implemented"

asPut :: Request -> Either Request ()
asPut (Put k v) = Left $ Put k v
asPut _ = Right ()

kvs :: Choreo (Kleisli IO) () (Response @ Client)
kvs =
  -- client prepare and send the request
  client `locally0` getRequest >>>
  client ~> primary >>>
  (&&&)
    -- primary handle the request
    (primary `locally` handleRequest)
    -- disparate behavior based on the request
    (cond' primary (arr asPut) $
      (|||)
        -- propogate Put k v
        (arr wrap >>>
        primary ~> backup >>>
        backup `locally` handleRequest >>>
        discard)
        -- do nothing with Get k
        discard
    ) >>>
  arr fst >>>
  primary ~> client

epp_kvs_client :: Network (Kleisli IO) () (Response @ Client)
epp_kvs_client = epp kvs $ toLocTm client

epp_kvs_primary :: Network (Kleisli IO) () (Response @ Client)
epp_kvs_primary = epp kvs $ toLocTm primary

epp_kvs_backup :: Network (Kleisli IO) () (Response @ Client)
epp_kvs_backup = epp kvs $ toLocTm backup

main :: IO ()
main = pure ()

