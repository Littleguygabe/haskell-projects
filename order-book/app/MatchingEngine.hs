module MatchingEngine where

import Types
import Control.Monad.State
import qualified Data.Map as Map

match :: Order -> State MarketState (Maybe Order)
match o = do
    ms <- get
    let (newBook, fills) = matchPure o (orderBook ms)
        totalFilled = sum $ map fillQty fills
        remainingQty = quantity o - totalFilled
    put ms {orderBook = newBook, filledOrders = fills ++ filledOrders ms}
    if remainingQty > 0
        then return (Just o {quantity = remainingQty})
        else return Nothing


matchPure :: Order -> OrderBookState -> (OrderBookState,[Fill])
matchPure o obs = case side o of
    Bid -> matchAgainst o obs (Map.lookupMin(askOrders obs)) askOrders (\obs' market -> obs' {askOrders = market})
    Ask -> matchAgainst o obs (Map.lookupMax(bidOrders obs)) bidOrders (\obs' market -> obs' {bidOrders = market})


-- by taking a getter and a setter for the market we're trading against it provides expandability

matchAgainst :: Order -> OrderBookState -> Maybe (Price, [Order]) 
                        -> (OrderBookState -> Market)
                        -> (OrderBookState -> Market -> OrderBookState)
                        -> (OrderBookState,[Fill])
matchAgainst _ obs Nothing _ _= (obs, [])
matchAgainst o obs (Just (bestPrice, restingOrders)) gtr str =
    if not (crosses o bestPrice)
        then (obs, [])
        else
            let (fills, remainingOrders) = handleFill o restingOrders
                newObs = updateLevel remainingOrders bestPrice obs gtr str
                remainingQty = quantity o - sum (map fillQty fills)

            in if remainingQty > 0 && null remainingOrders
                then let (obs', fills') = matchPure (o {quantity = remainingQty}) newObs
                    in (obs', fills ++ fills')
                else (newObs, fills)

crosses :: Order -> Price -> Bool
crosses o p = case side o of 
            Bid -> price o >= p
            Ask -> price o <= p 

updateLevel :: [Order] -> Price -> OrderBookState 
                                -> (OrderBookState -> Market)
                                -> (OrderBookState -> Market -> OrderBookState)
                                -> OrderBookState
updateLevel ro p obs gtr str = 
    let updatedOrders = if null ro 
        then Map.delete p (gtr obs)
        else Map.insert p ro (gtr obs)
    in str obs updatedOrders

handleFill :: Order -> [Order] -> ([Fill],[Order])
handleFill _ [] = ([],[])
handleFill io (ro:ros)
    | quantity io > quantity ro = (
        Fill bidId askId (price ro) (quantity ro) : remainingFills,
        remainingOrders
    )
    | quantity io < quantity ro = (
        [Fill bidId askId (price ro) (quantity io)],
        (ro {quantity = quantity ro - quantity io}) : ros
    )
    | otherwise = (
        [Fill bidId askId (price ro) (quantity ro)],
        ros
    )
    where
        (remainingFills, remainingOrders) = handleFill (io {quantity = quantity io - quantity ro}) ros
        (bidId, askId) = case side io of
            Bid -> (orderId io, orderId ro)
            Ask -> (orderId ro, orderId io)