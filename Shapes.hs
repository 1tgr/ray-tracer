module Shapes where

import Debug.Trace
import MathUtils

type Shader = Direction -> Position -> Normal -> Color

data Material = Material
	{
		shader :: Shader,
		reflection :: Float,
		transmission :: Float,
		refractiveIndex :: Float
	}

data Hit = Hit
	{
		t :: Float,
		normal :: Normal,
		material :: Material
	}

data Ray = Ray Position Direction

data Light 
	= PointLight Position

data Shape
	= Sphere Position Float Material
	| Plane Normal Float Material

rayPoint (Ray start direction) t = 
	start `add` (direction `scale` t)

epsilon = 0.001

intersect (Sphere centre radius material) ray @ (Ray start direction) =
	map intersection $ filter (> epsilon) (roots a b c)
	where
		a = squareMagnitude direction
		b = 2 * direction `dot` (start `sub` centre)
		c = squareMagnitude (start `sub` centre) - radius ^ 2
		normal t = ((rayPoint ray t) `sub` centre) `scale` (1.0 / radius)
		intersection t = Hit { t = t, normal = normal t, material = material }

intersect (Plane normal distance material) (Ray start direction)
	| vd > 0 && t > epsilon = [ Hit { t = t, normal = neg normal, material = material } ]
	| vd < 0 && t > epsilon = [ Hit { t = t, normal = normal, material = material } ]
	| otherwise = [ ]
	where
		vd = normal `dot` direction
		v0 = negate ((normal `dot` start) + distance)
		t = v0 / vd
