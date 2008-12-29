module Shapes where

import Debug.Trace
import MathUtils

data Material = Material
	{
		shader :: Ray -> Hit -> Color,
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
	let
		a = squareMagnitude direction
		b = 2 * direction `dot` (start `sub` centre)
		c = squareMagnitude (start `sub` centre) - radius ^ 2
		intersection t =
			Hit { t = t, normal = normal, material = material }
			where normal = ((rayPoint ray t) `sub` centre) `scale` (1.0 / radius)
	in map intersection $ filter (> epsilon) (roots a b c)

intersect (Plane normal distance material) (Ray start direction) =
	let
		vd = normal `dot` direction
		v0 = negate ((normal `dot` start) + distance)
	in
		if vd == 0
		then [ ]
		else
			let	t = v0 / vd in
			if t > epsilon
			then [ Hit { t = t, normal = (if vd > 0 then neg normal else normal), material = material } ]
			else [ ]
