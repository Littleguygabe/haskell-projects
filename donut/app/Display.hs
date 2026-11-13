module Display where

import Data.Array
import Types

scaleY :: Double
scaleY = 100

scaleX :: Double
scaleX = 150

gain :: Double
gain = 1.2

asciiRamp :: String
asciiRamp = ".,-~:;=!*#$@"
-- asciiRamp = "@$#*!=;:~-,."



getLChar :: Double -> Char
getLChar l = asciiRamp !! idx
  where
    clamped = max 0 (min 0.99 (l*gain))
    idx = floor (clamped * fromIntegral (length asciiRamp))

project :: Point3d -> Int -> Int -> ((Int, Int), Double)
project (x, y, z) tw th = ((col, row), z)
  where
    k2 = 0.1* fromIntegral (tw+th)
    ooz = 1 / (z + k2) 
    
    col = round (fromIntegral tw / 2 + scaleX * ooz * x)
    row = round (fromIntegral th / 2 + scaleY * ooz * y)

normalize :: Point3d -> Point3d
normalize (x, y, z) = (x / m, y / m, z / m)
  where
    m = sqrt (x**2 + y**2 + z**2)

lightingPos :: Point3d
lightingPos = normalize (1.0, 0.0, -1.0)

dotProd3d :: Point3d -> Point3d -> Double
dotProd3d (x1, y1, z1) (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2

luminocity :: Surface -> LuminocitySurface
luminocity s = [(p, dotProd3d n lightingPos)| (p, n) <- s]

charMap :: LuminocitySurface -> CharSurface
charMap s = [(p, getLChar (l * gain)) | (p, l) <- s]

buildCanvas :: Int -> Int -> CharSurface -> DisplayCanvas
buildCanvas tw th s = accumArray combiner initial canvasBounds associations
  where
    combiner = zBuffer
    initial  = (' ', -1e9)
    canvasBounds = ((0, 0), (th - 1, tw - 1)) 
    associations = [ ((row, col), (c, z)) 
                   | (p, c) <- s
                   , let ((col, row), z) = project p tw th
                   , col >= 0 && col < tw
                   , row >= 0 && row < th
                   ]
zBuffer :: (Char, Double) -> (Char, Double) -> (Char, Double)
zBuffer (oc, oz) (nc, nz) = if nz > oz then (nc, nz) else (oc, oz)


render :: DisplayCanvas -> IO ()
render canvas = putStr $ unlines $ chunksOf width allChars
  where
    ((_, _), (_, maxX)) = bounds canvas
    width = maxX + 1
    allChars = elems (fmap fst canvas)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)