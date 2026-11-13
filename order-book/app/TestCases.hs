module TestCases where

import Types
import MarketInteractions
import Control.Monad.State

runTests :: IO()
runTests = do
    runTestHelper "Original Test Orders" testOrders
    runTestHelper "Simple Match (10 @ 100 vs 10 @ 100)" testSimpleMatch
    runTestHelper "Partial Match (20 @ 100 vs 10 @ 100)" testPartialMatch
    runTestHelper "Price Priority (Bid 110 should hit Ask 100 first)" testPricePriority
    runTestHelper "Time Priority (Two Asks @ 100, Bid hits first one first)" testTimePriority
    runTestHelper "Cancellation (Cancel Bid before Ask arrives)" testCancellation
    runTestHelper "Market Sweep" testSweep

runTestHelper :: String -> State MarketState () -> IO ()
runTestHelper name test = do
    putStrLn $ "\n" ++ replicate 20 '='
    putStrLn $ "TEST: " ++ name
    let market = execState test newMarket
    print market

testOrders :: State MarketState ()
testOrders = do
    placeOrder Bid 100 10
    placeOrder Bid 100 11
    placeOrder Ask 30 15
    placeOrder Bid 5 10

testSimpleMatch :: State MarketState ()
testSimpleMatch = do
    placeOrder Bid 10 100
    placeOrder Ask 10 100

testPartialMatch :: State MarketState ()
testPartialMatch = do
    placeOrder Bid 20 100
    placeOrder Ask 10 100

testPricePriority :: State MarketState ()
testPricePriority = do
    placeOrder Ask 10 110
    placeOrder Ask 10 100
    placeOrder Bid 10 110 -- Should match the 100 price level first

testTimePriority :: State MarketState ()
testTimePriority = do
    placeOrder Ask 10 100 -- First at price
    placeOrder Ask 10 100 -- Second at price
    placeOrder Bid 15 100 -- Should fully fill the first, partially fill the second

testCancellation :: State MarketState ()
testCancellation = do
    placeOrder Bid 10 100
    cancelOrder 1 -- IDs start at 1
    placeOrder Ask 10 100 -- Should NOT match anything

testSweep :: State MarketState ()
testSweep = do
    placeOrder Ask 10 100
    placeOrder Ask 10 110
    placeOrder Bid 20 110 