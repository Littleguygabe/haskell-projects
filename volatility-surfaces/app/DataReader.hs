{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module DataReader where

-- Uses inline python to be able to access yfinance
-- Wants to follow the pattern of Initialise -> Execute -> Extract

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import GHC.Generics (Generic)

-- the '!' forces haskell to evaluate the data, rather than being lazy and
-- creating thunks for each data which can fill memory incredibly quickly
data DayData = DayData
    {
        date :: !String,
        open :: !Double,
        high :: !Double,
        low :: !Double,
        close :: !Double,
        volume :: !Int,
        dividends :: !Double,
        stockSplits :: !Double
    } deriving (Show, Generic)


-- Explicitly mapping from the fields in 'DayData' to the headers in the csv file
instance FromNamedRecord DayData where
    parseNamedRecord :: NamedRecord -> Parser DayData
    parseNamedRecord r =
        DayData
            <$> r .: "Date"        
            <*> r .: "Open"        
            <*> r .: "High"        
            <*> r .: "Low"         
            <*> r .: "Close"       
            <*> r .: "Volume"      
            <*> r .: "Dividends"    
            <*> r .: "Stock Splits" 

dataFolder :: String
dataFolder = "data/cachedData/"

-- tells cassava to map CSV headers to field names

readTickerCSVFile :: String -> IO (Either String (V.Vector DayData))
readTickerCSVFile ticker = do
    csvData <- BL.readFile (dataFolder ++ ticker ++ ".csv")
    return $ case decodeByName csvData of
        Left err -> Left ("Data Not Found" ++ err)
        Right (_,v) -> return v


-- takes a target ticker and gets a given variable for that ticker, ie AAPL Close
extractVariable :: (DayData -> b) -> String -> IO (Either String (V.Vector b))
extractVariable var ticker = do
    csvData <- readTickerCSVFile ticker
    return $ case csvData of 
        -- Left or right side of the either data type
        Left err -> Left ("Data Not Found" ++ err)
        Right v  -> Right (V.map var v)
