{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE Arrows    #-}

-- | This module implments the HTTP message transport backend for the `Network`
-- monad.
module ChoreographyArrow.Network.Http where

import ChoreographyArrow.Location
import ChoreographyArrow.Network
import Data.ByteString (fromStrict)
import Data.Proxy (Proxy(..))
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HashMap
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Servant.API
import Servant.Client (ClientM, client, runClientM, BaseUrl(..), mkClientEnv, Scheme(..))
import Servant.Server (Handler, Server, serve)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Control.Arrow.ArrowIO
import Control.Arrow.FreerArrow
import Control.Arrow
import Control.Category
import Data.Profunctor
import Prelude hiding ((.), id)

-- * Servant API

type API = "send" :> Capture "from" LocTm :> ReqBody '[PlainText] String :> PostNoContent

-- * Http configuration

-- | The HTTP backend configuration specifies how locations are mapped to
-- network hosts and ports.
newtype HttpConfig = HttpConfig
  { locToUrl :: HashMap LocTm BaseUrl
  }

type Host = String
type Port = Int

-- | Create a HTTP backend configuration from a association list that maps
-- locations to network hosts and ports.
mkHttpConfig :: [(LocTm, (Host, Port))] -> HttpConfig
mkHttpConfig = HttpConfig . HashMap.fromList . fmap (fmap f)
  where
    f :: (Host, Port) -> BaseUrl
    f (host, port) = BaseUrl
      { baseUrlScheme = Http
      , baseUrlHost = host
      , baseUrlPort = port
      , baseUrlPath = ""
      }

locs :: HttpConfig -> [LocTm]
locs = HashMap.keys . locToUrl

-- * Receiving channels

type RecvChans = HashMap LocTm (Chan String)

mkRecvChans :: HttpConfig -> IO RecvChans
mkRecvChans cfg = foldM f HashMap.empty (locs cfg)
  where
    f :: HashMap LocTm (Chan String) -> LocTm
      -> IO (HashMap LocTm (Chan String))
    f hm l = do
      c <- newChan
      return $ HashMap.insert l c hm

-- * HTTP backend

--TODO
runNetworkHttp :: ArrowIO ar => HttpConfig -> LocTm -> Network ar b a -> ar b a
runNetworkHttp cfg self prog = proc b -> do
  mgr <- arrIO0 $ liftIO $ newManager defaultManagerSettings -< ()
  chans <- arrIO (\cfg -> liftIO $ mkRecvChans cfg) -< cfg
  recvT <- arrIO (\(cfg, chans) -> liftIO $ forkIO (recvThread cfg chans)) -< (cfg, chans)
  -- result
  arrIO0 $ liftIO $ threadDelay 1000000 -< () -- wait until all outstanding requests to be completed
  arrIO $ (\recvT -> liftIO $ killThread recvT) -< recvT
  returnA -< _
--   result <- runNetworkMain prog -< (mgr, chans)
--   returnA -< _
  where
    runNetworkMain :: ArrowIO ar => Network ar (Manager, RecvChans) a -> ar () a
    runNetworkMain mgr chans = interp handler
      where
        handler :: ArrowIO ar => NetworkSig ar (Manager, RecvChans) a -> ar () a
        handler (Run ar) = ar
        handler (Send l) = _ -- proc a -> do -- liftIO $ do
--           res <- arrIO $ (\a -> runClientM (send self $ show a) (mkClientEnv mgr (locToUrl cfg ! l))) -< a
--           case res of
--             Left err -> arrIO $ (\err -> putStrLn $ "Error : " ++ show err) -< err
--             Right _ -> returnA -< ()
        handler (Recv l) = arrIO0 $ liftIO $ read <$> readChan (chans ! l)
-- --         handler BCast    = arr (\a -> mapM_ handler $ fmap Send (locs cfg))
-- 
    api :: Proxy API
    api = Proxy

    send :: LocTm -> String -> ClientM NoContent
    send = client api

    server :: RecvChans -> Server API
    server chans = handler
      where
        handler :: LocTm -> String -> Handler NoContent
        handler rmt msg = do
          liftIO $ writeChan (chans ! rmt) msg
          return NoContent

    recvThread :: HttpConfig -> RecvChans -> IO ()
    recvThread cfg chans = _ -- run (baseUrlPort $ locToUrl cfg ! self ) (serve api $ server chans)

instance Backend HttpConfig where
  runNetwork = runNetworkHttp
