module Shapes where

import Lighting
import MathUtils

data Material = Material
  {
    shader :: Shader,
    reflection :: Float,
    transmission :: Float,
    refractiveIndex :: Float
  }

data Intersection = Intersection
  {
    t :: Float,
    normal :: Normal,
    material :: Material
  }

data Ray = Ray Position Direction

data Shape = Sphere Position Float Material
           | Plane Normal Float Material

rayPoint :: Ray -> Float -> Position
rayPoint (Ray start direction) t = start `add` (direction `scale` t)

epsilon :: Float
epsilon = 0.001

intersect :: Shape -> Ray -> [ Intersection ]
intersect (Sphere centre radius material) ray @ (Ray start direction) = [intersection r | r <- roots a b c, r > epsilon ]
  where a = squareMagnitude direction
        b = 2 * direction `dot` (start `sub` centre)
        c = squareMagnitude (start `sub` centre) - (radius * radius)
        normal t = normalize ((rayPoint ray t) `sub` centre)
        intersection t = Intersection { t = t, normal = normal t, material = material }

intersect (Plane normal distance material) (Ray start direction) | vd > 0 && t > epsilon = [ Intersection { t = t, normal = neg normal, material = material } ]
                                                                 | vd < 0 && t > epsilon = [ Intersection { t = t, normal = normal, material = material } ]
                                                                 | otherwise = [ ]
  where vd = normal `dot` direction
        v0 = negate ((normal `dot` start) + distance)
        t = v0 / vd
