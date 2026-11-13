module Main where
import Control.Monad.State
import Types
import MarketInteractions
import TestCases


-- just need to start the market
main :: IO ()
main = runStateT marketRunner newMarket >> return ()

marketRunner :: StateT MarketState IO ()
marketRunner = do
    command <- liftIO getLine
    case command of
        "Show"  -> do
            s <- get
            liftIO $ print s
        _ ->
            case parseExchangeCommand command of
                Just action -> modify (execState action)
                Nothing -> liftIO $ putStrLn $ unlines ["Unknow Command, Usage:",
                                            "   (Bid/Ask) Quantity Price",
                                            "   Cancel OrderId",
                                            "   Show"]
    marketRunner