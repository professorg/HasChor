{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Choreography (runChoreography)
import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Http
import Data.Proxy

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

getRequest :: IO Request
getRequest = getLine >>= pure . read

handleRequest :: Request -> IO Response
handleRequest = error "not implemented"

kvs :: Choreo IO (Response @ Client)
kvs = do
  -- client prepare and send the request
  req <- client `locally` \_ -> getRequest
  req' <- (client, req) ~> primary

  -- primary handle the request
  res <- primary `locally` \unwrap -> handleRequest $ unwrap req'
  -- disparate behavior based on the request
  cond (primary, req') \case
    Get k -> return ()
    Put k v -> do
      -- primary propagate and backup handle the request
      req'' <- (primary, req') ~> backup
      backup `locally` \unwrap -> handleRequest $ unwrap req''
      return ()

  (primary, res) ~> client

main :: IO ()
main = pure ()

