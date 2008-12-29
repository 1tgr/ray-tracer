module Scene where

import MathUtils
import Phong
import Shapes

scene =
	[
		Sphere (Vector (-1) 0 0) 1 $ shiny grey, 
		Sphere (Vector 1 0 0) 1.5 $ flat red, 
		Plane (Vector 0 (-1) 0) (-1) $ shiny blue
	]
	where
		shiny colour = Material { shader = phong ambient white colour exponent diffuse specular lights, reflection = 0.9, transmission = 0, refractiveIndex = 0 }
		flat colour = Material { shader = (\_ _ -> colour), reflection = 0, transmission = 0, refractiveIndex = 0 }
		ambient = 0
		red = Color 1 0 0
		blue = Color 0 0 1
		white = Color 1 1 1
		grey = Color 0.5 0.5 0.5
		exponent = 10
		diffuse = 1
		specular = 1
		lights = [ PointLight $ Vector (-10) 10 (-10), PointLight $ Vector 10 0 0 ]
