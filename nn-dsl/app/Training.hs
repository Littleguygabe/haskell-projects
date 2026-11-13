odule Training where

import Numeric.LinearAlgebra

import NetworkStructure

type LossFunction = Vector R -> Vector R -> Double

-- take a network and input vector of values
-- then output a prediction

-- ? check input vector and network input layer are the same size  

--
getInputs :: Vector R
getInputs = vector [1..10]
--

trainStep :: Network -> Vector R -> Vector R -> LossFunction -> (Double, Network)
trainStep n i e lf =
  let vectorList = forwardPropagation n i [i]
      prediction = head vectorList
      loss = lf e prediction
      initialError = prediction - e
      forwardsList = reverse vectorList      
      (_, updatedNet) = backPropagation n 0.01 forwardsList initialError

  in (loss, updatedNet)

validateInput :: Int -> Vector R -> Bool
validateInput i v
  | i == size v = True
  | otherwise = False


forwardPropagation :: Network -> Vector R -> [Vector R]-> [Vector R]
forwardPropagation (Final _) v acc = v : acc
forwardPropagation (Connected (Edge w) _ n) v acc = forwardPropagation n output (output: acc)
  where output = tanh (v <# w) 

standardiseVector :: Vector R -> Vector R
standardiseVector v = diff/scalar sig
  where
    n = fromIntegral (size v)
    mu = sumElements v / n
    diff = v - scalar mu
    var = (diff <.> diff) / (n-1)
    sig = sqrt var

mse :: LossFunction
mse e p =
  let diff = p - e
  in sumElements (diff ** 2) / fromIntegral (size e)

-- use chain rule to identify derivates
backPropagation :: Network -> Double -> [Vector R] -> Vector R -> (Vector R, Network)
backPropagation (Final l) _ _ initialError = (initialError, Final l)
backPropagation (Connected (Edge w) l nextNet) learnRate (a_in : rest) initialError = 
    let (errorFromAhead, updatedNextNet) = backPropagation nextNet learnRate rest initialError
        weightGradient = outer a_in errorFromAhead
        updatedW = w - scale learnRate weightGradient
        errorToPassBack = (errorFromAhead <# tr' w) * cmap (\x -> 1 - x*x) a_in
    in (errorToPassBack, Connected (Edge updatedW) l updatedNextNet)

backPropagation _ _ _ _ = error "Fatal: Activation history list was too short during backprop!"
