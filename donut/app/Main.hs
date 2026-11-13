module Main where

import Display
import Donut
import Control.Concurrent (threadDelay)
import Types
import Cube 

main :: IO ()
main = putStrLn "Hello, Haskell!"

drawDonut :: Int -> Int -> Double -> Double -> Maybe Rotation -> IO()
drawDonut tw th r1 r2 Nothing =
    let rs = createDonut r1 r2
        ls = luminocity rs
        cs = charMap ls
        dc = buildCanvas tw th cs
    in render dc
drawDonut tw th r1 r2 (Just r) =
    let rs = createDonut r1 r2
        rs' = rotateSurface rs r
        ls = luminocity rs'
        cs = charMap ls
        dc = buildCanvas tw th cs
    in render dc


animateCube :: Int -> Int -> Double -> IO()
animateCube tw th w = animateHelper 0.0 0.0 (createCube w) tw th

animateDonut :: Int -> Int -> Double -> Double -> IO()
animateDonut tw th r1 r2 = animate (createDonut r1 r2) tw th

displayStaticCube :: Int -> Int -> Double -> IO ()
displayStaticCube tw th w =
    let s = createCube w
        ls = luminocity s
        cs = charMap ls
        dc = buildCanvas tw th cs
    in render dc

animate :: Surface -> Int -> Int -> IO()
animate = animateHelper 0.0 0.0 

animateHelper :: Double -> Double -> Surface -> Int -> Int -> IO ()
animateHelper a b bs tw th = do
    -- 1. Move cursor to top-left to overwrite the previous frame
    putStr "\ESC[H"
    
    -- 2. Create and rotate the donut surface using your existing functions
    let rotatedX = rotateSurface bs (X, a)
        rotatedZ = rotateSurface rotatedX (Z, b)

        ls = luminocity rotatedZ
        cs = charMap ls
        dc = buildCanvas tw th cs
    
    render dc
    threadDelay 0
    animateHelper (a+0.05) (b - 0.05) bs tw th 