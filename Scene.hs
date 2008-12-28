module Scene where

import MathUtils
import Phong
import Shapes

scene =
	[
		Sphere (Vector 0 0 0) 1 shiny, 
		Plane (Vector 0 (-1) 0) (-0.1) flat
	]
	where
		shiny = phong ambient specularColour surfaceColour exponent diffuse specular lights
		flat _ _ = surfaceColour
		ambient = 0
		specularColour = Color 1 1 0 0
		surfaceColour = Color 1 1 0 0
		exponent = 4
		diffuse = 1
		specular = 1
		lights = [ PointLight $ Vector (-10) 10 (-10), PointLight $ Vector 10 0 0 ]
