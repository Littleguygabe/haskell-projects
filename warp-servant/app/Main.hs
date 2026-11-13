{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai.Handler.Warp
import Servant

import GHC.Generics
import Data.Aeson
import Control.Monad.IO.Class 

main :: IO()
main = do
    putStrLn "Running Server"
    run 8000 (serve (Proxy :: Proxy TestAPI) server)

-- defining a http route as a type
-- so has 3 components:
    -- 1. the path (/postOrder)
    -- 2. a method (POST, GET, ...)
    -- 3. what it returns

data Confirmation = Accept | Fail deriving (Generic)
instance ToJSON Confirmation

data Message = Message{
    title :: String,
    body :: [String]
} deriving (Generic, Show)
instance ToJSON Message
instance FromJSON Message where
  parseJSON = genericParseJSON defaultOptions

-- ':>' is like saying 'then'
-- so in this case we get
    -- when a call is made to 'postOrder' return a json confirmation value
type TestAPI = "postOrder" :> Post '[JSON] Confirmation

-- Saying the input from 'recvMsg' will be in the format of the Message data type
-- then return a Confirmation
                :<|> "recvMsg" :> ReqBody '[JSON] Message :> Post '[JSON] Confirmation


server :: Server TestAPI
server = returnAccept :<|> printReturnAccept

returnAccept :: Handler Confirmation
returnAccept = pure Accept

printReturnAccept :: Message -> Handler Confirmation
printReturnAccept m = liftIO (print m) *> pure Accept