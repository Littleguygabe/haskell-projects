module Main where

import Server

import Numeric.LinearAlgebra

-- Analyst Side of the system - ie what they calculate roughly

data Side = BUY | SELL
type Volume = Int
type Price = Double
type TradeID = Int

data Action = MarketOrder Side Volume
  | LimitOrder Side Price Volume
  | CancelOrder Volume TradeID 

type Observation = Int
type BeliefState = Vector Double
type TransitionMatrix = Matrix Double
type Ack = (Bool, String)

----- Analysts develop from here down -----

-- updates the transition matrix tracking of probabilities of moving into
-- another given regime from our current one
updateTransition :: TransitionMatrix -> Observation -> TransitionMatrix
updateTransition cMatrix obs = undefined

-- update the vector (belief state) representing the probability of currently being in any given regime
updateBelief :: TransitionMatrix -> BeliefState -> Action -> Observation -> BeliefState
updateBelief cMatrix cBelief lastAction obs = undefined

-- perform a trade based on the current belief state
applyTradePolicy :: BeliefState -> Action
applyTradePolicy cBelief = undefined

-- some function to receive observations from the exchange
recvObservation :: IO Observation
recvObservation = undefined
-- just some function to receive observations from the CLOB/exchange

-- some function to post orders/execute actions on the exchange
executeAction :: Action -> IO Ack
executeAction action = undefined

systemStep :: (BeliefState, TransitionMatrix) -> Action -> IO ()
systemStep (cBelief, cMatrix) lastAction = do
    obs <- recvObservation
    let nMatrix = updateTransition cMatrix obs
    -- applyTradePolicy would likely execute its trade in IO here too
    let nBelief = updateBelief nMatrix cBelief lastAction obs
    let action = applyTradePolicy nBelief
    ack <- executeAction action
    systemStep (nBelief, nMatrix) action


-- main func else cabal has a hissy fit
main :: IO()
main = putStrLn "Hello world!"
