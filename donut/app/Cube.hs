module Cube where

import Types

resolution :: Double
resolution = 0.5

createCube :: Double -> Surface
createCube w = concat [createFace a d w | a <- [X, Y, Z], d <- [-1.0, 1.0]]

-- create 2 faces at the same time, ie if we get X in, make +ve and -ve x at the same time

createFace :: Axis -> Double -> Double -> Surface
createFace a d w = case a of
    X -> [((d * w/2, y, z), (d, 0, 0)) | y <- [l, l + resolution .. u], z <- [l, l + resolution .. u]]
    Y -> [((x, d * w/2, z), (0, d, 0)) | x <- [l, l + resolution .. u], z <- [l, l + resolution .. u]]
    Z -> [((x, y, d * w/2), (0, 0, d)) | x <- [l, l + resolution .. u], y <- [l, l + resolution .. u]]
    where
        u = w/2
        l = -u

    
