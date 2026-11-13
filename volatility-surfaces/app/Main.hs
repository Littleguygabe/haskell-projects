module Main where

import Graphing
import DataReader

main :: IO ()
main = do
    -- Plotting multiple tickers using mapM
    plotTickers ["AAPL", "MSFT", "GOOG"]

plotTickers :: [String] -> IO ()
plotTickers tickers = do
    -- mapM ensure each value is IO wrapped
    -- fmap applies the extract variable close operation and wraps the output from that in a
    -- TSVector, so we get an output of [IO (TSVector)]

    -- basically fmap lets us apply a function to a wrapped value then re-wrap it
        -- so allows the whole thing to work

    allSeries <- mapM (fmap TSVector . extractVariable close) tickers
    
    plotLine allSeries
