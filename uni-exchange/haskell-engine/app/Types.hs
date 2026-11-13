{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where
    
import qualified Data.Map as Map
import Data.Aeson
import Data.Aeson.Types 
import GHC.Generics (Generic)

data Side = Bid | Ask deriving (Show, Read, Eq)

instance ToJSON Side where
    toJSON Bid = toJSON ("Bid" :: String)
    toJSON Ask = toJSON ("Ask" :: String)

instance FromJSON Side where
    parseJSON v = case v of
        String t
            | t == "Bid" -> pure Bid
            | t == "Ask" -> pure Ask
            | otherwise -> typeMismatch "Side" v
        _ -> typeMismatch "Side" v

type OrderId = Int
type Qty = Int
type Price = Int

type Market = Map.Map Price [Order]
type OrderIndex = Map.Map OrderId (Price,Side)

data Order = Order{
    orderId :: OrderId,
    side :: Side,
    price :: Price,
    quantity :: Qty
} deriving (Show)

instance ToJSON Order where
    toJSON o = object
        [
        "orderId" .= orderId o,
        "side" .= side o,
        "price" .= price o,
        "quantity" .= quantity o
        ]   

data OrderRequest = OrderRequest{
    reqSide :: Side,
    reqPrice :: Price,
    reqQuantity :: Qty
} deriving (Generic)

instance Show OrderRequest where
    show o = show (reqSide o) ++ " " ++ show (reqQuantity o) ++ " @ " ++ show (reqPrice o)

instance FromJSON OrderRequest where
    parseJSON = withObject "OrderRequest" $ \o -> OrderRequest
        <$> o .: "side"
        <*> o .: "volume"
        <*> o .: "price"

data Fill = Fill{
    bidOrderId :: OrderId,
    askOrderId :: OrderId,
    fillPrice :: Price,
    fillQty :: Qty
}

instance Show Fill where
    show f =
        " Bid Order Id: " ++ show (bidOrderId f) ++ 
        " Ask Order Id: " ++ show (askOrderId f) ++ 
        " Fill Price: " ++ show (fillPrice f) ++ 
        " Fill Qty: " ++ show (fillQty f) ++
        "\n"

instance ToJSON Fill where
    toJSON f = object
        [ "bidOrderId" .= bidOrderId f
        , "askOrderId" .= askOrderId f
        , "fillPrice"  .= fillPrice f
        , "fillQty"    .= fillQty f
        ]

data OrderBookState = OrderBookState{
    bidOrders :: Market,
    askOrders :: Market,
    orderIndex :: OrderIndex
}

instance Show OrderBookState where
    show book = unlines
        ["Asks:",
        showSide (askOrders book),
        "Bids:",
        showSide (bidOrders book)
        ]

showSide :: Market -> String
showSide m = unlines [
        "  " ++ show p ++ " -> " ++ show (length orders) ++ " order(s), qty: " ++ show (sum $ map quantity orders)
        | (p, orders) <- Map.toDescList m
    ]

data MarketState = MarketState{
    orderBook :: OrderBookState,
    currentOrderId :: Int,
    filledOrders :: [Fill]
}

instance ToJSON MarketState where
    toJSON ms = object
        [ "bids" .= Map.toList (bidOrders (orderBook ms))
        , "asks" .= Map.toList (askOrders (orderBook ms))
        , "fills" .= filledOrders ms
        ]

instance Show MarketState where
    show ms = unlines [
        "===== Current Market State =====",
        show (orderBook ms),
        "Fills:"
        ] ++ concatMap show (filledOrders ms)

emptyOrderBook :: OrderBookState
emptyOrderBook = OrderBookState{
    bidOrders = Map.empty,
    askOrders = Map.empty,
    orderIndex = Map.empty
}

newMarket :: MarketState
newMarket = MarketState{
    orderBook = emptyOrderBook,
    currentOrderId = 0,
    filledOrders = []
}

data ConfirmationStatus = Accepted | Rejected deriving (Generic)
instance ToJSON ConfirmationStatus

data Confirmation = Confirmation{
    status :: ConfirmationStatus,
    message :: Maybe String
} deriving (Generic)
instance ToJSON Confirmation 