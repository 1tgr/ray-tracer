module Tracing where

import Data.Maybe
import Data.Ord
import Debug.Trace
import List(minimumBy)
import MathUtils
import Shapes

data Hit = Hit { depth :: Int,
                 color :: Color,
                 ray :: Ray,
                 intersection :: Intersection,
                 secondaryHits :: [ Hit ] }

eyeRay :: Float -> Float -> Ray
eyeRay x y = Ray (cameraPos `add` point) $ normalize (point `sub` cameraPos)
  where cameraPos = Vector 0 0 (-10)
        point = Vector x y 0

reflectedRay :: Ray -> Intersection -> Ray
reflectedRay ray_ @ (Ray _ direction) Intersection { t = t, normal = normal } = Ray (rayPoint ray_ t) $ reflection
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
closest list = Just $ minimumBy (comparing t) list

black :: Color
black = Color 0 0 0

unpackTraceResult :: Maybe Hit -> (Int, Color)
unpackTraceResult Nothing = (0, black)
unpackTraceResult (Just Hit { depth = d, color = c }) = (d, c)

cast :: [ Shape ] -> Int -> Float -> Ray -> Maybe Hit
cast _ depth @ 15 _ _ = Nothing
cast scene depth refractiveIndex ray @ (Ray _ direction) = shade 
                                                         $ foldl intersectScene [ ]
                                                         $ scene
  where intersectScene list shape = (intersect shape ray) ++ list
        directionToViewer = neg direction
        shade intersections = do
          intersection <- closest intersections
          let Intersection { t = t, 
                             normal = normal, 
                             material = Material { shader = shader, 
                                                   reflection = reflection, 
                                                   transmission = transmission, 
                                                   refractiveIndex = newRefractiveIndex } } = intersection
          let castSecondaryRay = cast scene (depth + 1) newRefractiveIndex
          let reflectionResult | reflection > 0 = castSecondaryRay $ reflectedRay ray intersection
                               | otherwise = Nothing
          let transmissionResult | transmission > 0 = transmittedRay refractiveIndex newRefractiveIndex ray intersection >>= castSecondaryRay
                                 | otherwise = Nothing
          let (reflectedDepth, reflectedColour) = unpackTraceResult reflectionResult
          let (transmittedDepth, transmittedColour) = unpackTraceResult transmissionResult
          let surfaceColour = shader directionToViewer (rayPoint ray t) normal
          return $ Hit { depth = max reflectedDepth transmittedDepth,
                         color = (blend (1 - transmission) surfaceColour (transmittedColour `scale` transmission)) `add` (reflectedColour `scale` reflection),
                         ray = ray,
                         intersection = intersection,
                         secondaryHits = catMaybes [ reflectionResult, transmissionResult ] }

trace :: [ Shape ] -> Float -> Float -> Maybe Hit
trace scene x y = cast scene 0 1 $ eyeRay x y
