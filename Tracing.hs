module Tracing where

import Debug.Trace
import MathUtils
import Shapes

eyeRay x y =
	Ray (cameraPos `add` point) $ normalize (point `sub` cameraPos)
	where
		cameraPos = Vector 0 0 (-10)
		point = Vector x y 0

reflectedRay ray @ (Ray _ direction) hit @ (Hit { t = t, normal = normal }) =
	Ray (rayPoint ray t) $ reflection
	where
		c1 = -(direction `dot` normal)
		reflection = direction `add` (normal `scale` (2 * c1))

transmittedRay n1 n2 ray @ (Ray _ direction) hit @ (Hit { t = t, normal = normal }) =
	if sinT2 > 1
	then
		let message = foldl (++) [ ] 
			[
				"transmittedRay: total internal reflection: n1/n2 = ", show n1, "/", show n2,
				", normal = ", show normal,
				", direction = ", show direction,
				", cosI = ", show cosI,
				", sinT2 = ", show sinT2 
			] in
		Debug.Trace.trace message Nothing
	else Just $ Ray (rayPoint ray t) refraction
	where
		n = n1 / n2
		cosI = normal `dot` direction
		sinT2 = n * n * (1 - (cosI * cosI))
		refraction = (direction `scale` n) `sub` (normal `scale` (n + sqrt (1 - sinT2)))

closest [ ] = Nothing
closest (x:xs) =
	Just $ foldl selectNearest x xs
	where selectNearest hit1 @ Hit { t = t1 } hit2 @ Hit { t = t2 }
		| t1 < t2 = hit1
		| otherwise = hit2

black = Color 0 0 0

cast _ depth @ 15 _ _ = (depth, black)
cast scene depth refractiveIndex ray =
	(shade . closest) (foldl intersectScene [ ] scene)
	where
		intersectScene list shape = (intersect shape ray) ++ list
		shade Nothing = (depth, black)
		shade (Just hit) =
			(
				max reflectedDepth transmittedDepth,
				(blend (1 - transmission) surfaceColour (transmittedColour `scale` transmission)) `add` (reflectedColour `scale` reflection)
			)
			where
				Ray _ direction = ray
				Hit { t = t, normal = normal, material = Material { shader = shader, reflection = reflection, transmission = transmission, refractiveIndex = newRefractiveIndex } } = hit
				directionToViewer = neg direction
				hitPoint = rayPoint ray t
				castSecondaryRay newRay =
					let (Ray _ newDirection) = newRay in
					let message = foldl (++) [ ]
						[
							"castSecondaryRay: depth = ", show depth,
							", t = ", show t,
							", n1/n2 = ", show refractiveIndex, "/", show newRefractiveIndex,
							", direction = ", show direction, " -> ", show newDirection
						] in
					Debug.Trace.trace message $ cast scene (depth + 1) newRefractiveIndex newRay
				surfaceColour = shader directionToViewer hitPoint normal
				(reflectedDepth, reflectedColour)
					| reflection > 0 = castSecondaryRay $ reflectedRay ray hit
					| otherwise = (depth, black)
				(transmittedDepth, transmittedColour)
					| transmission > 0 = 
						case transmittedRay refractiveIndex newRefractiveIndex ray hit of
						Nothing -> (depth, black)
						Just r -> castSecondaryRay r
					| otherwise = (depth, black)

trace scene x y =
	cast scene 0 1 $ eyeRay x y
