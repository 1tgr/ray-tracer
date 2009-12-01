module Tracing where

import Data.Maybe
import Debug.Trace
import MathUtils
import Shapes

data Hit = Hit
  {
    depth :: Int,
    color :: Color,
    ray :: Ray,
    intersection :: Intersection,
    secondaryHits :: [ Hit ]
  }

eyeRay :: Float -> Float -> Ray
eyeRay x y = Ray (cameraPos `add` point) $ normalize (point `sub` cameraPos)
  where cameraPos = Vector 0 0 (-10)
        point = Vector x y 0

reflectedRay :: Ray -> Intersection -> Ray
reflectedRay ray @ (Ray _ direction) Intersection { t = t, normal = normal } =Ray (rayPoint ray t) $ reflection
  where c1 = -(direction `dot` normal)
        reflection = direction `add` (normal `scale` (2 * c1))

transmittedRay :: Float -> Float -> Ray -> Intersection -> Maybe Ray
transmittedRay n1 n2 ray @ (Ray _ direction) Intersection { t = t, normal = normal }
  | magnitude normal > 1.0001 = error ("got non-normalized normal: | " ++ show normal ++ " | = " ++ show (magnitude normal))
  | magnitude direction > 1.0001 = error ("got non-normalized direction: | " ++ show direction ++ " | = " ++ show (magnitude direction))
  | magnitude refraction > 1.0001 = error ("got non-normalized refraction: | " ++ show refraction ++ " | = " ++ show (magnitude refraction))
  | sinT2 > 1 = let message = foldl (++) [ ] [ "transmittedRay: total internal reflection: n1/n2 = ", show n1, "/", show n2,
                                               ", normal = ", show normal,
                                               ", direction = ", show direction,
                                               ", cosI = ", show cosI,
                                               ", sinT2 = ", show sinT2 ]
                in Debug.Trace.trace message Nothing
  | otherwise = Just $ Ray (rayPoint ray t) refraction
  where n = n1 / n2
        cosI = normal `dot` direction
        sinT2 = n * n * (1 - (cosI * cosI))
        refraction = (direction `scale` n) `sub` (normal `scale` ((n * cosI) + sqrt (1 - sinT2)))

closest :: [ Intersection ] -> Maybe Intersection
closest [ ] = Nothing
closest (x:xs) = Just $ foldl selectNearest x xs
  where selectNearest i1 @ Intersection { t = t1 } i2 @ Intersection { t = t2 } | t1 < t2 = i1
                                                                                | otherwise = i2

black :: Color
black = Color 0 0 0

unpackTraceResult :: Maybe Hit -> (Int, Color)
unpackTraceResult Nothing = (0, black)
unpackTraceResult (Just Hit { depth = d, color = c }) = (d, c)

cast :: [ Shape ] -> Int -> Float -> Ray -> Maybe Hit
cast _ depth @ 15 _ _ = Nothing
cast scene depth refractiveIndex ray = (shade . closest) (foldl intersectScene [ ] scene)
  where intersectScene list shape = (intersect shape ray) ++ list
        shade Nothing = Nothing
        shade (Just intersection) = Just Hit { depth = max reflectedDepth transmittedDepth,
                                               color = (blend (1 - transmission) surfaceColour (transmittedColour `scale` transmission)) `add` (reflectedColour `scale` reflection),
                                               ray = ray,
                                               intersection = intersection,
                                               secondaryHits = catMaybes [ reflectionResult, transmissionResult ] }
          where Ray _ direction = ray
                Intersection { t = t, normal = normal, material = Material { shader = shader, reflection = reflection, transmission = transmission, refractiveIndex = newRefractiveIndex } } = intersection
                directionToViewer = neg direction
                hitPoint = rayPoint ray t
                castSecondaryRay newRay = cast scene (depth + 1) newRefractiveIndex newRay
                surfaceColour = shader directionToViewer hitPoint normal
                reflectionResult | reflection > 0 = castSecondaryRay $ reflectedRay ray intersection
                                 | otherwise = Nothing
                (reflectedDepth, reflectedColour) = unpackTraceResult reflectionResult
                transmissionResult | transmission > 0 = case transmittedRay refractiveIndex newRefractiveIndex ray intersection of
                                                        Nothing -> Nothing
                                                        Just r -> castSecondaryRay r
                                   | otherwise = Nothing
                (transmittedDepth, transmittedColour) = unpackTraceResult transmissionResult

trace :: [ Shape ] -> Float -> Float -> Maybe Hit
trace scene x y = cast scene 0 1 $ eyeRay x y
