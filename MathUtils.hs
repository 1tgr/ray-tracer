{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
module MathUtils where

data Floating a => NumVector a = NumVector a a a
                                 deriving (Eq, Show)

data Floating a => NumColor a = NumColor a a a

class Magnitude a b | a -> b where
  scale :: a -> b -> a
  squareMagnitude :: a -> b
  dot :: a -> a -> b

class Magnitude a b => Vector a b | a -> b where
  fromXYZ :: b -> b -> b -> a
  add :: a -> a -> a
  sub :: a -> a -> a
  neg :: a -> a
  normalize :: a -> a

class Floating b => Color a b | a -> b where
  fromRGB :: b -> b -> b -> a
  blend :: b -> a -> a -> a

magnitude :: Floating b => Magnitude a b => a -> b
magnitude v = sqrt $ squareMagnitude v

square :: Num a => a -> a
square n = n * n

xor :: Bool -> Bool -> Bool
xor True b = not b
xor False b = b

instance Floating a => Magnitude (NumColor a) a where
  scale (NumColor r g b) x = NumColor (r * x) (g * x) (b * x)
  squareMagnitude (NumColor r g b) = r * r + g * g + b * b
  dot (NumColor r1 g1 b1) (NumColor r2 g2 b2) = r1 * r2 + g1 * g2 + b1 * b2

instance Floating a => Vector (NumColor a) a where
  fromXYZ r g b = NumColor r g b
  add (NumColor r1 g1 b1) (NumColor r2 g2 b2) = NumColor (r1 + r2) (g1 + g2) (b1 + b2)
  sub (NumColor r1 g1 b1) (NumColor r2 g2 b2) = NumColor (r1 - r2) (g1 - g2) (b1 - b2)
  neg (NumColor r g b) = NumColor (-r) (-g) (-b)
  normalize c @ (NumColor r g b) = let m = magnitude c in NumColor (r / m) (g / m) (b / m)

instance Floating a => Color (NumColor a) a where
  fromRGB r g b = NumColor r g b
  blend a c1 c2 = (c1 `scale` a) `add` (c2 `scale` (1 - a))

instance Floating a => Magnitude (NumVector a) a where
  scale (NumVector x y z)  n = NumVector (x * n) (y * n) (z * n)
  squareMagnitude (NumVector x y z) = x * x + y * y + z * z
  dot (NumVector x1 y1 z1) (NumVector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

instance Floating a => Vector (NumVector a) a where
  fromXYZ x y z = NumVector x y z
  add (NumVector x1 y1 z1) (NumVector x2 y2 z2) = NumVector (x1 + x2) (y1 + y2) (z1 + z2)
  sub (NumVector x1 y1 z1) (NumVector x2 y2 z2) = NumVector (x1 - x2) (y1 - y2) (z1 - z2)
  neg (NumVector x y z) = NumVector (-x) (-y) (-z)
  normalize v @ (NumVector x y z) = let m = magnitude v in NumVector (x / m) (y / m) (z / m)

roots :: (Floating a, Ord a) => a -> a -> a -> [ a ]
roots a b c | discriminant < 0 = [ ]
            | discriminant > 0 = [ (-b - sqrt discriminant) / (2 * a), (-b + sqrt discriminant) / (2 * a)]
            | otherwise = [ -b / (2 * a)]
            where discriminant = (square b) - 4 * a * c
