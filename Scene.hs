module Scene(scene) where

import Lighting
import MathUtils
import Shapes

white :: Color colour num => colour
white = fromRGB 1 1 1

flatShader :: colour -> Shader vector colour
flatShader colour _ _ _ = colour

checkedShader :: (Floating num, RealFrac num) => Shader (NumVector num) colour -> Shader (NumVector num) colour -> Shader (NumVector num) colour
checkedShader shader1 shader2 hitPoint @ (NumVector x y z) | xeven `xor` yeven `xor` zeven = shader1 hitPoint
                                                           | otherwise = shader2 hitPoint
  where xeven = even (truncate (x * 100.0) :: Integer)
        yeven = even (truncate (y * 100.0) :: Integer)
        zeven = even (truncate (z * 100.0) :: Integer)

scene :: (Color colour num, Floating num, RealFrac num) => [ Shape (NumVector num) colour num ]
scene = [ Sphere (fromXYZ (-1) 0 0) 1 $ flat grey,
          Sphere (fromXYZ 1 0 0) 1 $ flat red,
          Plane (fromXYZ 0 0 (-1)) 10 $ flatChecked blue white ]
  where flat colour = Material { shader = flatShader colour, reflection = 0, transmission = 0, refractiveIndex = 1 }
        flatChecked colour1 colour2 = Material { shader = checkedShader (flatShader colour1) (flatShader colour2), reflection = 0, transmission = 0, refractiveIndex = 1 }
        red = fromRGB 1 0 0
        blue = fromRGB 0 0 1
        grey = fromRGB 0.5 0.5 0.5
