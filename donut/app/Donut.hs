module Donut where

import Types

createDonut :: Double -> Double -> Surface
createDonut r1 r2 = [createDonutSurfacePoint u v r1 r2 | u <- [0.0, 0.1 .. 2 * pi], v <- [0.0, 0.1 .. 2 * pi]]

-- this also allows us to create other shapes, so long as they return a surface

createDonutSurfacePoint :: Double -> Double -> Double -> Double -> (Point3d, Point3d)
createDonutSurfacePoint u v r1 r2 = ((x, y, z),(nx, ny ,nz))
    where
        x = (r2 + r1 * cos u) * cos v
        y = r1 * sin u
        z = -((r2 + r1 * cos u) * sin v)
        nx = cos u * cos v
        ny = sin u
        nz = (- cos u) * sin v

rotateSurface :: Surface -> Rotation -> Surface
rotateSurface s (a, d) =  [(f p, f n) | (p, n) <- s]
    where 
        f = case a of
            X -> rotatePointX d
            Y -> rotatePointY d
            Z -> rotatePointZ d

rotatePointX :: Double -> Point3d -> Point3d
rotatePointX r (x, y, z) = (x, y' ,z')
    where 
        y' = y * cos r - z * sin r
        z' = y * sin r + z * cos r

rotatePointY :: Double -> Point3d  -> Point3d
rotatePointY r (x, y, z) = (x', y ,z')
    where 
        x' = x * cos r + z * sin r
        z' = -(x * sin r) + z * cos r

rotatePointZ :: Double -> Point3d  -> Point3d
rotatePointZ r (x, y, z) = (x', y' ,z)
    where 
        x' = x * cos r - y * sin r
        y' = x * sin r + y * cos r

deg2rad :: Double -> Double
deg2rad d = d * (pi/180) 
