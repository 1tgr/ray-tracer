module Tracing where

import MathUtils
import Shapes

cast _ Nothing = Color 0 0 0 0
cast ray (Just (hit @ (Hit _ _ shader))) = 
	shader ray hit

intersectScene _ [ ] = [ ]
intersectScene ray (shape : scene) = (intersect shape ray) ++ (intersectScene ray scene)

closest [ ] = Nothing
closest (x:xs) =
	Just $ foldl selectNearest x xs
	where selectNearest hit1 @ (Hit t1 _ _) hit2 @ (Hit t2 _ _) = if t1 < t2 then hit1 else hit2

trace scene x y =
	cast ray $ closest (intersectScene ray scene)
	where
		cameraPos = Vector 0 0 (-10)
		point = Vector x y 0
		ray = Ray (cameraPos `add` point) $ normalize (point `sub` cameraPos)
