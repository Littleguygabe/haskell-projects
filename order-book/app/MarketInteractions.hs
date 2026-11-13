module MarketInteractions where

import Types
import Control.Monad.State
import qualified Data.Map as Map
import MatchingEngine
import Text.Read (readMaybe)

freshOrderId :: State MarketState Int
freshOrderId = do
    s <- get
    let newId = currentOrderId s + 1
    put s {currentOrderId = newId}
    return newId

placeOrder :: Side -> Qty -> Price -> State MarketState ()
placeOrder sd sz p = do
    currentId <- freshOrderId
    let o = Order currentId sd p sz
    remaining <- match o

    case remaining of 
        Nothing -> return ()
        Just ro -> do
            modify $ \ms -> ms {orderBook = insertNewOrder ro (orderBook ms)}

insertNewOrder :: Order -> OrderBookState -> OrderBookState
insertNewOrder o obs = 
    let updatedIndex = Map.insert (orderId o) (price o, side o) (orderIndex obs)
        insertInto = Map.insertWith (flip (++)) (price o) [o]
    in case side o of
        Bid -> obs {bidOrders = insertInto (bidOrders obs), orderIndex = updatedIndex}
        Ask -> obs {askOrders = insertInto (askOrders obs), orderIndex = updatedIndex}
        

cancelOrder :: OrderId -> State MarketState ()
cancelOrder oid = do
    book <- gets orderBook
    case Map.lookup oid (orderIndex book) of
        Nothing -> return ()
        Just (p,sd) -> modify $ \ms -> ms {orderBook = removeResting sd p oid (orderBook ms)}

removeResting :: Side -> Price -> OrderId -> OrderBookState -> OrderBookState
removeResting s p oid book =
    let updateOrders = removeOrder p oid
        updateIndex = Map.delete oid (orderIndex book)
    in case s of 
        Bid -> book {bidOrders = updateOrders (bidOrders book), orderIndex = updateIndex}
        Ask -> book {askOrders = updateOrders (askOrders book), orderIndex = updateIndex}

removeOrder :: Price -> OrderId -> Market -> Market
removeOrder p oid = Map.update (\os -> case filter (\o -> orderId o /= oid) os of
    [] -> Nothing
    xs -> Just xs) p

parseExchangeCommand :: String -> Maybe (State MarketState ())
parseExchangeCommand command = case words command of
    [sd, q, p] ->
        case (readMaybe sd :: Maybe Side,
            readMaybe q :: Maybe Qty,
            readMaybe p :: Maybe Price) of
            (Just Ask, Just q', Just p') -> Just (placeOrder Ask q' p')
            (Just Bid, Just q', Just p') -> Just (placeOrder Bid q' p')
            _ -> Nothing

    ["Cancel", oid] ->
        case (readMaybe oid :: Maybe OrderId) of
            Just oid' -> Just (cancelOrder oid')
            _ -> Nothing
    _ -> Nothing
