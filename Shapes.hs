module Shapes where

import Lighting
import MathUtils

data (Vector vector num, Color colour num) => Material vector colour num = Material { shader :: Shader vector colour,
                                                                                      reflection :: num,
                                                                                      transmission :: num,
                                                                                      refractiveIndex :: num }

data (Vector vector num, Color colour num) => Intersection vector colour num = Intersection { t :: num,
                                                                                              normal :: vector,
                                                                                              material :: Material vector colour num }

data Vector vector num => Ray vector num = Ray vector vector

data (Vector vector num, Color colour num) => Shape vector colour num = Sphere vector num (Material vector colour num)
                                                                      | Plane vector num (Material vector colour num)

rayPoint :: Vector vector num => Ray vector num -> num -> vector
rayPoint (Ray start direction) t = start `add` (direction `scale` t)

intersect :: (Floating num, Ord num, Vector vector num, Color colour num) => Shape vector colour num -> Ray vector num -> [ Intersection vector colour num ]
intersect (Sphere centre radius material) ray @ (Ray start direction) = [ intersection r | r <- roots a b c, r > 0.001 ]
  where a = squareMagnitude direction
        b = 2 * direction `dot` (start `sub` centre)
        c = squareMagnitude (start `sub` centre) - (radius * radius)
        normal t = normalize ((rayPoint ray t) `sub` centre)
        intersection t = Intersection { t = t, normal = normal t, material = material }

intersect (Plane normal distance material) (Ray start direction) | vd > 0 && t > 0.001 = [ Intersection { t = t, normal = neg normal, material = material } ]
                                                                 | vd < 0 && t > 0.001 = [ Intersection { t = t, normal = normal, material = material } ]
                                                                 | otherwise = [ ]
  where vd = normal `dot` direction
        v0 = negate ((normal `dot` start) + distance)
        t = v0 / vd
