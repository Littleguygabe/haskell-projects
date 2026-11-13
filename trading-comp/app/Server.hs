module Server where

import Numeric.LinearAlgebra

serverMain :: IO()
serverMain = putStrLn "Hello from the server side code"

-- need to define the market graph structure

rowNormalise :: Vector Double -> Vector Double
rowNormalise r = cmap (\x -> x / sumElements r) r

-- define the initial transition matrix
