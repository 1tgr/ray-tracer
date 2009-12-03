module Tracing where

import Data.Maybe
import Data.Ord
import Debug.Trace
import List(minimumBy)
import MathUtils
import Shapes

data Hit vector colour num = Hit { depth :: Int,
                                   color :: colour,
                                   ray :: Ray vector num,
                                   intersection :: Intersection vector colour num,
                                   secondaryHits :: [ Hit vector colour num ] }

eyeRay :: (Num num, Vector vector num) => num -> num -> Ray vector num
eyeRay x y = Ray (cameraPos `add` point) $ normalize (point `sub` cameraPos)
  where cameraPos = fromXYZ 0 0 (-10)
        point = fromXYZ x y 0

reflectedRay :: (Color colour num, Vector vector num) => Ray vector num -> Intersection vector colour num -> Ray vector num
reflectedRay ray_ @ (Ray _ direction) Intersection { t = t, normal = normal } = Ray (rayPoint ray_ t) $ reflection
  where c1 = -(direction `dot` normal)
        reflection = direction `add` (normal `scale` (2 * c1))

transmittedRay :: (Color colour num, Ord num, Show vector, Vector vector num) => num -> num -> Ray vector num -> Intersection vector colour num -> Maybe (Ray vector num)
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

closest :: (Color colour num, Ord num, Vector vector num) => [ Intersection vector colour num ] -> Maybe (Intersection vector colour num)
closest [ ] = Nothing
closest list = Just $ minimumBy (comparing t) list

black :: Color colour num => colour
black = fromRGB 0 0 0

unpackTraceResult :: Color colour num => Maybe (Hit vector colour num) -> (Int, colour)
unpackTraceResult Nothing = (0, black)
unpackTraceResult (Just Hit { depth = d, color = c }) = (d, c)

cast :: (Color colour num, Ord num, Show vector, Vector colour num, Vector vector num) => [ Shape vector colour num ] -> Int -> num -> Ray vector num -> Maybe (Hit vector colour num)
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

trace :: (Color colour num, Ord num, Show vector, Vector colour num, Vector vector num) => [ Shape vector colour num ] -> num -> num -> Maybe (Hit vector colour num)
trace scene x y = cast scene 0 1 $ eyeRay x y
