module Types where
    
import qualified Data.Map as Map

data Side = Bid | Ask deriving (Show, Read, Eq)

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