module Phong(phong) where

import MathUtils
import Tracing
import Shapes

phong ambient specularColour surfaceColour exponent diffuse specular lights directionToViewer hitPoint normal =
	foldl add (surfaceColour `scale` ambient) (map phong' lights)
	where phong' (PointLight lightPoint) = 
		(surfaceColour `scale` diffuseFactor) `add` (specularColour `scale` specularFactor)
		where
			directionToLight = normalize $ neg (hitPoint `sub` lightPoint)
			diffuseFactor = diffuse * (max 0 $ directionToLight `dot` normal)
			h = (directionToLight `add` directionToViewer) `scale` 0.5
			specularFactor = specular * ((max 0 $ normal `dot` h) ** exponent)
