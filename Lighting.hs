module Lighting where

import MathUtils
  
type Shader = Direction -> Position -> Normal -> Color

data Light = PointLight Position
