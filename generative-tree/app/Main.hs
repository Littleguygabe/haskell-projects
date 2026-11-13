module Main where
import Control.Monad.State
import qualified Data.Map as M

type Point = (Int, Int)
type AsciiPos = (Int, Int, Char)
data Direction = N | NE | E | SE | S | SW | W | NW

data TreeCom = F Int
                |X
                |TurnRight
                |TurnLeft
                |Push
                |Pop 
                |End
  deriving (Eq)


instance Show TreeCom where
  show (F n) = show n
  show X = ""
  show TurnRight = "TurnRight"
  show TurnLeft = "TurnLeft"
  show Push = "["
  show Pop = "]"
  show End = "End"

data TurtleState = TurtleState{
  currentPos :: Point,
  currentDirection :: Direction,
  stack :: [(Point,Direction)],
  output :: [AsciiPos]
  }

type TreeStack = [TreeCom]


main :: IO ()
main = do
  putStrLn "Generating Fractal Commands..."
  let commands = producer [F 1] 4 bushyMutator
      finalState = execState (consume commands) initState
      asciiData = output finalState
  render asciiData  

initState :: TurtleState
initState = TurtleState{
  currentPos = (0,0),
  currentDirection = N,
  stack = [],
  output = []
}

snowflakeMutator :: TreeCom -> [TreeCom]
snowflakeMutator (F n) = [F n, Push, TurnRight, F n, Pop, Push, TurnLeft, F n,Pop]
snowflakeMutator x = [x]

treeMutator :: TreeCom -> [TreeCom]
treeMutator (F n) = [F n]
treeMutator X = [F 1, Push, TurnRight, X, Pop, Push, TurnLeft, X, Pop]
treeMutator x = [x]

bushyMutator :: TreeCom -> [TreeCom]
bushyMutator (F n) = 
  [ F n
  , Push, TurnRight, F n, Pop
  , F n
  , Push, TurnLeft, F n, Pop
  , Push, F n, Pop
  ]
bushyMutator x = [x]

producer :: [TreeCom] -> Int -> (TreeCom -> [TreeCom]) -> [TreeCom]
producer tcs n mutator =
  case n of
    0 -> tcs
    _ -> producer (concatMap mutator tcs) (n-1) mutator

directionToStep :: Direction -> (Int, Int)
directionToStep N = (0,1)
directionToStep NE = (1,1)
directionToStep E = (1,0)
directionToStep SE = (1,-1)
directionToStep S = (0,-1)
directionToStep SW = (-1,-1)
directionToStep W = (-1,0)
directionToStep NW = (-1,1)


directionToChar :: Direction -> Char
directionToChar N ='|'
directionToChar NE ='/'
directionToChar E ='~'
directionToChar SE ='%'
directionToChar S ='|'
directionToChar SW ='/'
directionToChar W ='~'
directionToChar NW ='%'

rightRotation :: Direction -> Direction
rightRotation N = NE
rightRotation NE = E
rightRotation E = SE
rightRotation SE = S
rightRotation S = SW
rightRotation SW = W
rightRotation W = NW
rightRotation NW = N

leftRotation :: Direction -> Direction
leftRotation N = NW
leftRotation NE = N
leftRotation E = NE
leftRotation SE = E
leftRotation S = SE
leftRotation SW = S
leftRotation W = SW
leftRotation NW = W

consume :: [TreeCom] -> State TurtleState ()
consume [] = return ()
consume (hd:tl) = case hd of
  (F 0) -> consume tl
  (F n) -> do
    s <- get
    let (cx,cy) = currentPos s
        cd = currentDirection s
        o = output s
        (xStep, yStep) = directionToStep cd
        ac = directionToChar cd
        (nx,ny) = (cx+xStep, cy+yStep)
    
    put (s {output = (nx,ny,ac):o, currentPos = (nx,ny)})
    consume (F (n-1):tl)
    
  TurnRight -> do
    s <- get
    let cd = currentDirection s
    put (s {currentDirection = rightRotation cd})
    consume tl
    
  TurnLeft -> do
    s <- get
    let cd = currentDirection s
    put (s {currentDirection = leftRotation cd})
    consume tl
    
  Push -> do
    s <- get
    let cp = currentPos s
        cd = currentDirection s
        st = stack s
    put (s {stack = (cp,cd) : st})
    consume tl
  
  Pop -> do
    s <- get
    let st = stack s
        (np,nd,st') = case st of
          ((shp,shd):ros) -> (shp,shd,ros)
          _ -> (currentPos s,currentDirection s,[])
    put (s {currentPos = np, currentDirection = nd, stack = st'})
    consume tl
  
  End -> consume tl
  X -> consume tl

extractBounds :: [AsciiPos] -> (Int,Int,Int,Int)
extractBounds aps = (maximum xs, maximum ys, minimum xs, minimum ys)
  where
    xs = map extractx aps
    ys = map extracty aps
    
extractx :: AsciiPos -> Int
extractx (x,_,_) = x

extracty :: AsciiPos -> Int
extracty (_,y,_) = y

buildArray :: [AsciiPos] -> [[Char]]
buildArray aps =
  let (uX, uY, lX, lY) = extractBounds aps
      gridMap = M.fromList [((x,y),c) | (x,y,c) <- aps]
      findChar x y = case M.lookup (x,y) gridMap of
        Just c -> c
        Nothing -> ' '

      buildRow y = [findChar x y | x <- [lX .. uX]]
  in [buildRow y | y <- [uY, uY - 1 .. lY]]
  
render :: [AsciiPos] -> IO()
render aps = do
  putStrLn "Rendering the Tree"
  let o = buildArray aps
  mapM_ putStrLn o
  

  
