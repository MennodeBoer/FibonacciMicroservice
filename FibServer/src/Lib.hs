{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib where

import Data.Aeson
import Data.Aeson.TH
import Data.Either
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.Reader        (ReaderT, runReaderT, asks, liftIO)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import GHC.Generics
import Servant
import Control.Concurrent.STM
import Network.HTTP.Client hiding (Proxy)
import Servant.Client
import Servant.Client.Core
import Control.Monad.Except

data PortNumber = One | Two

readPort :: PortNumber -> Int
readPort One = 8081
readPort Two = 8082

swapPort :: PortNumber -> PortNumber
swapPort One = Two
swapPort Two = One

type FibAPI = "api" :> ("current" :> (Get '[JSON] Int
                                     :<|> ReqBody '[JSON] Int :> Post '[JSON] String)                                                
                        :<|> "next" :> Post '[JSON] String)

fibApi :: Proxy FibAPI
fibApi = Proxy 

fibApp :: TVar Int -> Manager -> PortNumber -> Application
fibApp var manager portnum = serve fibApi (fibServer var manager portnum)

fibServer :: TVar Int ->  Manager -> PortNumber -> Server FibAPI
fibServer var manager portnum = (getCurrentServer var
                                :<|> postCurrentServer var)
                                :<|> postNextServer var manager portnum

getCurrentServer ::  TVar Int -> Handler Int
getCurrentServer var = liftIO $ atomically $ readTVar var


postCurrentServer :: TVar Int -> Int -> Handler String
postCurrentServer var y = do liftIO $ atomically $ modifyTVar var (+y)
                             return "succes!"

postNextServer :: TVar Int -> Manager -> PortNumber -> Handler String
postNextServer var manager portnum = do x <- liftIO $ atomically $ readTVar var
                                        res <- liftIO $ runClientM (postClient x) (mkClientEnv manager (BaseUrl Http "localhost" (readPort (swapPort portnum)) "/api/current"))
                                        either microserviceError return res
      
microserviceError :: ClientError -> Handler a
microserviceError e = do liftIO (putStrLn ("Got internal-api error: " ++ show e))
                         throwError $ err500 {errBody = "CyberInternal MicroServer MicroError"}



type PostApi = ReqBody '[JSON] Int :> Post '[JSON] String

postApi :: Proxy PostApi
postApi = Proxy

postClient :: Int -> ClientM String
postClient = client postApi

runFibApp :: PortNumber -> IO ()
runFibApp portnum = do var <- atomically $ newTVar 1
                       manager <- newManager defaultManagerSettings
                       run (readPort portnum) $ logStdoutDev $ (fibApp var manager portnum)
