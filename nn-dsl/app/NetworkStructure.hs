module NetworkStructure where

-- We add 'rows' and 'cols' to extract the dimensions of the matrix without printing the whole thing
import Numeric.LinearAlgebra

data Layer = HiddenLayer Int | OutputLayer Int | InputLayer Int
  deriving (Eq)

instance Show Layer where
  show (InputLayer n)  = "[Input Layer: " ++ show n ++ "]"
  show (HiddenLayer n) = "[Hidden Layer: " ++ show n ++ "]"
  show (OutputLayer n) = "[Output Layer: " ++ show n ++ "]"

newtype Edge = Edge {weights :: Matrix R}

instance Show Edge where
  show (Edge w) = "(Edge " ++ show (rows w) ++ "x" ++ show (cols w) ++ ")"
  
data Network =
  Final Layer
  | Connected Edge Layer Network

instance Show Network where
  show (Final l) = show l
  show (Connected e l n) = 
    show l ++ "\n" ++
    "       |\n" ++
    "  " ++ show e ++ "\n" ++
    "       v\n" ++
    show n

-- Input Neurons -> Num Rows
-- Output Neurons -> Num Cols

-- ? Need to add validation for 0 layer networks

buildNetwork :: [Int] -> IO Network
buildNetwork [] = return (error "Cannot build an empty Network")
buildNetwork [x] = return (Final (InputLayer x))
buildNetwork (x:y:xs) = do
  newEdge <- initEdge x y
  cNetwork <- buildNetwork (y:xs)

  return (Connected newEdge (HiddenLayer x) cNetwork)

initEdge :: Int -> Int -> IO Edge
initEdge i o = do
  rawMatrix <- randn i o
  let xScale = sqrt (2.0 / fromIntegral (i + o))
      scaledM = cmap (* xScale) rawMatrix

  return (Edge scaledM)
