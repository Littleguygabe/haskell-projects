{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Types
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Control.Monad.State
import MarketInteractions 
import Logger

type ExchangeAPI = "postOrder"      :> ReqBody '[JSON] OrderRequest :> Post '[JSON] Confirmation
              :<|> "orderBookState" :> Get '[JSON] MarketState

exchange :: TVar MarketState -> Server ExchangeAPI
exchange s = postOrderHandler s :<|> bookStateHandler s

postOrderHandler :: TVar MarketState -> OrderRequest -> Handler Confirmation
postOrderHandler s req = do
    if reqPrice req <= 0 || reqQuantity req <= 0
        then do
            return (Confirmation Rejected (Just "Price & Quantity Must be Positive"))
        else liftIO $ do
            atomically $ modifyTVar s $ \ms -> execState (placeOrder (reqSide req) (reqQuantity req) (reqPrice req)) ms
            return (Confirmation Accepted Nothing)


-- Handler is the servant's wrapper around IO that handles errors
bookStateHandler :: TVar MarketState -> Handler MarketState
bookStateHandler s = liftIO $ readTVarIO s

startServer :: IO ()
startServer = do
    stateVar <- newTVarIO newMarket
    putStrLn "Exchange running on port 8000"
    run 8000 (serve (Proxy :: Proxy ExchangeAPI) (exchange stateVar))