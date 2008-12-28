module Shapes where

import Debug.Trace
import MathUtils

data Hit = Hit Float Normal Shader
data Ray = Ray Position Direction
type Shader = Ray -> Hit -> Color

data Light 
	= PointLight Position

data Shape
	= Sphere Position Float Shader
	| Plane Normal Float Shader

rayPoint (Ray start direction) t = 
	start `add` (direction `scale` t)

intersect (Sphere centre radius shader) ray @ (Ray start direction) =
	let
		a = squareMagnitude direction
		b = 2 * direction `dot` (start `sub` centre)
		c = squareMagnitude (start `sub` centre) - radius ^ 2
		intersection t =
			Hit t normal shader
			where normal = ((rayPoint ray t) `sub` centre) `scale` (1.0 / radius)
	in map intersection $ filter (> 1e-6) (roots a b c)

intersect (Plane normal distance shader) (Ray start direction) =
	let
		vd = normal `dot` direction
		v0 = negate ((normal `dot` start) + distance)
	in
		if vd == 0
		then [ ]
		else
			let	t = v0 / vd in
			if t > 1e-6
			then [ Hit t (if vd > 0 then neg normal else normal) shader ]
			else [ ]
