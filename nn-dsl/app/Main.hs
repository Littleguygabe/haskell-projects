module Main where

import Numeric.LinearAlgebra
import NetworkStructure
import Training

trainLoop :: Int -> Int -> Network -> Vector R -> Vector R -> IO Network
trainLoop currentEpoch maxEpoch net input expected
  | currentEpoch >= maxEpoch = do
    putStrLn "Training Done."
    return net
  | otherwise = do
    let (loss, updatedNet) = trainStep net input expected mse
    putStrLn $ "Epoch " ++ show currentEpoch ++ " - Loss: " ++ show loss
    trainLoop (currentEpoch+1) maxEpoch updatedNet input expected

main :: IO ()
main = do
  initialNet <- buildNetwork [10,5,2]
  let input = standardiseVector (vector [1..10])
      expected = vector [0,1]
  putStrLn "Starting Training"
  finalNet <- trainLoop 0 1000 initialNet input expected
  putStrLn "Done"
  print finalNet
