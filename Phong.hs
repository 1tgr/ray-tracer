module Phong where

import MathUtils
import Tracing
import Shapes

phong :: Float -> Color -> Color -> Float -> Float -> Float -> [ Light ] -> Ray -> Hit -> Color
phong ambient _ surfaceColour _ _ _ [ ] _ _ = surfaceColour `scale` ambient
phong ambient specularColour surfaceColour exponent diffuse specular ((PointLight lightPoint) : lights) ray @ (Ray start direction) hit @ (Hit t normal _) =
    phong ambient specularColour surfaceColour exponent diffuse specular lights ray hit
		`add` (surfaceColour `scale` diffuse `scale` diffuseFactor)
		`add` ((specularColour `blend` surfaceColour) `scale` specular `scale` specularFactor)
	where
		directionToViewer = neg direction
		hitPoint = rayPoint ray t
		directionToLight = normalize $ neg (hitPoint `sub` lightPoint)
		diffuseFactor = max 0 $ directionToLight `dot` normal
		h = (directionToLight `add` directionToViewer) `scale` 0.5
		specularFactor = (max 0 $ normal `dot` h) ** exponent
