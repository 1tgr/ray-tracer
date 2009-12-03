module Lighting where

type Shader vector colour = vector -> vector -> vector -> colour

data Light vector = PointLight vector
