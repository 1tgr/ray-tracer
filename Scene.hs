module Scene(scene) where

import Lighting
import MathUtils
import Shapes

white :: Color
white = Color 1 1 1

flatShader :: Color -> Shader
flatShader colour _ _ _ = colour

checkedShader :: Shader -> Shader -> Shader
checkedShader shader1 shader2 hitPoint @ (Vector x y z) | xeven `xor` yeven `xor` zeven = shader1 hitPoint
                                                        | otherwise = shader2 hitPoint
  where xeven = even (truncate (x * 100.0) :: Integer)
        yeven = even (truncate (y * 100.0) :: Integer)
        zeven = even (truncate (z * 100.0) :: Integer) 

scene :: [ Shape ]
scene = [ Sphere (Vector (-1) 0 0) 1 $ flat grey,
          Sphere (Vector 1 0 0) 1 $ flat red,
          Plane (Vector 0 0 (-1)) 10 $ flatChecked blue white ]
  where flat colour = Material { shader = flatShader colour, reflection = 0, transmission = 0, refractiveIndex = 1 }
        flatChecked colour1 colour2 = Material { shader = checkedShader (flatShader colour1) (flatShader colour2), reflection = 0, transmission = 0, refractiveIndex = 1 }
        red = Color 1 0 0
        blue = Color 0 0 1
        grey = Color 0.5 0.5 0.5
