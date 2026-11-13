module Graphing (
    RangeSpec(..),
    TimeSeries(..),
    generateFunctionData,
    plotScatter,
    plotLine,
) where

import Granite.String
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

data RangeSpec = To Double | Between Double Double 

data TimeSeries
    = TSPoints [(Double, Double)]
    | TSVals [Double]
    | TSVector (Either String (V.Vector Double))

bounds :: RangeSpec -> (Double,Double)
bounds rs = case rs of
    To u -> (0.0,u)
    Between l u -> (l,u)

generateRange :: RangeSpec -> Double -> [Double]
generateRange rs step = [l, l + step .. u]
    where (l,u) = bounds rs

generateFunctionData :: RangeSpec -> Maybe Double -> (Double -> Double) -> TimeSeries
generateFunctionData rs maybeStep function = TSPoints (zip xRange yRange)
    where
        step = fromMaybe 1.0 maybeStep
        xRange = generateRange rs step
        yRange = map function xRange

plotScatter :: ([Double],[Double]) -> IO ()
plotScatter (xs,ys) = putStrLn (scatter [series "X-Axis" (zip xs ys)] defPlot{widthChars=68,heightChars=22,plotTitle="Test Scatter Graph"})

-- Takes an array of time series data and then labels each time series with 'stock n'
-- Then if the time series has X values we use those, otherwise we just create an index for the data

plotLine :: [TimeSeries] -> IO()
plotLine tss = putStrLn $ lineGraph dataPointList defPlot {plotTitle = "Simple Line Graph",widthChars=70}
    where
        tsPlotData = map pairData tss
        labels = ["Stock " ++ show (i :: Int) | i <- [1 .. ]]
        dataPointList = zip labels tsPlotData

pairData :: TimeSeries -> [(Double,Double)]
pairData ts = case ts of
    TSPoints xys -> xys
    TSVals ys -> zip [0..] ys
    TSVector eys -> case eys of
        Left _ -> [(0.0,0.0)]
        Right ys -> zip [0..] (V.toList ys)

