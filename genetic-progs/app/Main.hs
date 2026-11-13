module Main where
import System.Random


data Bin = Zero | One

newtype BinNum = BinNum [Bin]

instance Show BinNum where
  show (BinNum bits) = concatMap showBit bits
    where
      showBit Zero = "0"
      showBit One = "1"

type GeneticOp = BinNum -> BinNum

main :: IO()
main = print (BinNum [Zero,One,Zero])
