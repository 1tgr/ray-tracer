module Tracing where

import MathUtils
import Shapes

eyeRay x y =
	Ray (cameraPos `add` point) $ normalize (point `sub` cameraPos)
	where
		cameraPos = Vector 0 0 (-10)
		point = Vector x y 0

reflectedRay ray @ (Ray _ direction) hit @ (Hit { t = t, normal = normal }) =
	Ray (rayPoint ray t) $ reflection
	where reflection = neg $ (normal `scale` (2 * (normal `dot` direction))) `sub` direction

transmittedRay refractiveIndex ray @ (Ray _ direction) hit @ (Hit { t = t, normal = normal }) =
	Ray (rayPoint ray t) $ refraction
	where
		c1 = -(direction `dot` normal)
		c2 = sqrt (1 - (square refractiveIndex) * (1 - square c1))
		refraction = (direction `scale` refractiveIndex) `add` (normal `scale` (refractiveIndex * c1 - c2))

closest [ ] = Nothing
closest (x:xs) =
	Just $ foldl selectNearest x xs
	where selectNearest hit1 @ (Hit { t = t1 }) hit2 @ (Hit { t = t2 }) = if t1 < t2 then hit1 else hit2

black = Color 0 0 0

cast _ 10 _ = black
cast scene depth ray =
	bounce intersections
	where
		intersections = closest $ foldl (\ list shape -> (intersect shape ray) ++ list) [ ] scene
		bounce Nothing = black
		bounce (Just hit) =
			(blend (1 - transmission) surfaceColour transmittedColour) `add` reflectedColour
			where
				Hit { material = Material { shader = shader, reflection = reflection, transmission = transmission, refractiveIndex = refractiveIndex } } = hit
				castSecondaryRay rayFn = cast scene (depth + 1) $ rayFn ray hit
				surfaceColour = shader ray hit
				reflectedColour 
					| reflection > 0 = (castSecondaryRay reflectedRay) `scale` reflection
					| otherwise = black
				transmittedColour
					| transmission > 0 = (castSecondaryRay (transmittedRay refractiveIndex)) `scale` transmission
					| otherwise = black

trace scene x y =
	cast scene 0 $ eyeRay x y
