module Scene(scene) where

import MathUtils
import Phong
import Shapes

white = Color 1 1 1

shinyShader colour =
	phong ambient white colour exponent diffuse specular lights
	where
		ambient = 0
		exponent = 10
		diffuse = 1
		specular = 1
		lights = [ PointLight $ Vector (-10) 10 (-10), PointLight $ Vector 10 0 0 ]

flatShader colour _ _ _ =
	colour

checkedShader shader1 shader2 hitPoint @ (Vector x y z)
	| xeven `xor` yeven `xor` zeven = shader1 hitPoint
	| otherwise = shader2 hitPoint
	where
		xeven = even (truncate (x * 100.0))
		yeven = even (truncate (y * 100.0))
		zeven = even (truncate (z * 100.0)) 

scene =
	[
		Sphere (Vector (-1) 0 0) 1 $ shiny grey, 
		Sphere (Vector 1 0 0) 1.5 $ checked red white, 
		Plane (Vector 0 (-1) 0) (-1) $ checked blue white
	]
	where
		shiny colour = Material { shader = shinyShader colour, reflection = 0.9, transmission = 0, refractiveIndex = 0 }
		flat colour = Material { shader = flatShader colour, reflection = 0, transmission = 0, refractiveIndex = 0 }
		checked colour1 colour2 = Material { shader = checkedShader (shinyShader colour1) (shinyShader colour2), reflection = 0.9, transmission = 0, refractiveIndex = 0 }
		red = Color 1 0 0
		blue = Color 0 0 1
		grey = Color 0.5 0.5 0.5
