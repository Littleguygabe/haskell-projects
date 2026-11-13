module Types where

import Data.Array

data Axis = X | Y | Z

type Point3d = (Double,Double,Double)
type Point2d = (Double,Double)
type Canvas = [[Char]]
type Rotation = (Axis, Double)

type Surface = [(Point3d, Point3d)]

type LuminocitySurface = [(Point3d,Double)]
type CharSurface = [(Point3d,Char)]
type DisplayCanvas = Array (Int, Int) (Char, Double)

