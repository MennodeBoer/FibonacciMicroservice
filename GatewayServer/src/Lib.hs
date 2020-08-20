{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.Reader        (ReaderT, runReaderT, asks, liftIO)
import GHC.Generics
import Servant
import Control.Concurrent.STM
import Network.HTTP.Client hiding (Proxy)
import Servant.Client
import Servant.Client.Core
import Control.Monad.Except
import Data.Either

type GatewayApi = "current" :> Get '[JSON] Int
                :<|> "next" :> Post '[JSON] String


data PortNumber = One | Two

readPort :: PortNumber -> Int
readPort One = 8081
readPort Two = 8082

swapPort :: PortNumber -> PortNumber
swapPort One = Two
swapPort Two = One

gatewayApi :: Proxy GatewayApi
gatewayApi = Proxy 

gatewayApp :: TVar PortNumber -> Manager -> Application
gatewayApp var manager = serve gatewayApi (gatewayServer var manager)

gatewayServer :: TVar PortNumber ->  Manager -> Server GatewayApi
gatewayServer var manager = getCurrentServer var manager
                          :<|> getNextServer var manager

getCurrentServer ::  TVar PortNumber -> Manager -> Handler Int
getCurrentServer var manager = do port <- liftIO $ atomically $ readTVar var
                                  res <- liftIO $ runClientM getClient (mkClientEnv manager (BaseUrl Http "localhost" (readPort port) "/api/current"))
                                  either microserviceError return res

getNextServer :: TVar PortNumber -> Manager -> Handler String
getNextServer var manager = do port <- liftIO $ atomically $ readTVar var
                               liftIO $ atomically $ writeTVar var (swapPort port)
                               res  <- liftIO $ runClientM postClient (mkClientEnv manager (BaseUrl Http "localhost" (readPort (swapPort port)) "/api/next"))
                               either microserviceError return res
                               
microserviceError :: ClientError -> Handler a
microserviceError e = do liftIO (putStrLn ("Got internal-api error: " ++ show e))
                         throwError $ err500 {errBody = "CyberInternal MicroServer MicroError"}



type GetAPI = Get '[JSON] Int

getApi :: Proxy GetAPI
getApi = Proxy

getClient :: ClientM Int
getClient = client getApi

type PostApi = Post '[JSON] String

postApi :: Proxy PostApi
postApi = Proxy

postClient :: ClientM String
postClient = client postApi

runGatewayApp :: IO ()
runGatewayApp = do var <- atomically $ newTVar One
                   manager <- newManager defaultManagerSettings
                   run 8080 (gatewayApp var manager)
