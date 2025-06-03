{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Main where

import ChoreographyArrowChoice (runChoreography)
import ChoreographyArrowChoice.Choreo
import ChoreographyArrowChoice.Location
import ChoreographyArrowChoice.Network
import Data.Proxy
import GHC.TypeLits
import Control.Arrow
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Profunctor
import Control.Arrow.Freer.FreerChoiceArrow

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

participants :: Choreo ar b a -> Set LocTm
participants (Hom _) = Set.empty
participants (Comp _ e c) =
  participants c <>
  case e of
    Local l _ -> Set.singleton $ symbolVal l
    Comm l l' -> Set.fromList $ [symbolVal l, symbolVal l']
    Cond l c' -> participants c' <> Set.singleton (symbolVal l)

participants_next_cond :: Choreo ar b a -> Set LocTm
participants_next_cond (Hom _) = Set.empty
participants_next_cond (Comp _ (Cond l c) _) = participants c <> Set.singleton (symbolVal l)
participants_next_cond (Comp _ _ c) = participants_next_cond c

-- partial function; use only when you know x and y are the same value
mergeUnwrap :: (KnownSymbol l, KnownSymbol l', Eq a) => a @ l -> a @ l' -> a
mergeUnwrap (Wrap x) (Wrap y)
  | x == y = x

--TODO
-- rewrite_next_cond :: Choreo ar b a -> Choreo ar b a
-- rewrite_next_cond (Hom f) = Hom f
-- rewrite_next_cond (Comp f (Cond l c') c) =
--   let p = participants_next_cond $ Comp f (Cond l c') c in
--     -- I want something like `Comm l l1 &&& Comm l l2 &&& Comm l l3 &&& ...` where `p = [l1, l2, l3, ...]`
--     _
-- rewrite_next_cond (Comp f e c) =
--   if participants_next_cond (Comp f e c) == Set.empty
--   then Comp f e c
--   else Comp f e $ rewrite_next_cond c

epp_kvs_client :: Network (Kleisli IO) () (Response @ Client)
epp_kvs_client = epp kvs $ toLocTm client

epp_kvs_primary :: Network (Kleisli IO) () (Response @ Client)
epp_kvs_primary = epp kvs $ toLocTm primary

epp_kvs_backup :: Network (Kleisli IO) () (Response @ Client)
epp_kvs_backup = epp kvs $ toLocTm backup

main :: IO ()
main = pure ()

