module Phong(phong) where

import Lighting
import MathUtils

phong :: (Vector colour num, Vector vector num, Ord num, Floating num) => num -> colour -> colour -> num -> num -> num -> [ Light vector ] -> Shader vector colour
phong ambient specularColour surfaceColour exponent_ diffuse specular lights directionToViewer hitPoint normal = foldl add (surfaceColour `scale` ambient) (map phong' lights)
  where phong' light = (surfaceColour `scale` diffuseFactor) `add` (specularColour `scale` specularFactor)
          where PointLight lightPoint = light
                directionToLight = normalize $ neg (hitPoint `sub` lightPoint)
                diffuseFactor = diffuse * (max 0 $ directionToLight `dot` normal)
                h = (directionToLight `add` directionToViewer) `scale` 0.5
                specularFactor = specular * ((max 0 $ normal `dot` h) ** exponent_)
