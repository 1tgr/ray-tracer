module MathUtils where

data Vector =
	Vector Float Float Float 
	deriving (Eq, Show)

type Position = Vector
type Direction = Vector
type Normal = Vector
data Color = Color Float Float Float

class VectorOps a where
	scale :: a -> Float -> a
	add :: a -> a -> a
	dot :: a -> a -> Float
	sub :: a -> a -> a
	neg :: a -> a
	normalize :: a -> a
	squareMagnitude :: a -> Float

class (VectorOps a) => ColorOps a where
	blend :: Float -> a -> a -> a

magnitude :: VectorOps a => a -> Float
magnitude v = sqrt $ squareMagnitude v

square :: Num a => a -> a
square n = n * n

xor :: Bool -> Bool -> Bool
xor True b = not b
xor False b = b

instance VectorOps Color where
	scale (Color r g b) x = Color (r * x) (g * x) (b * x)
	add (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)
	dot (Color r1 g1 b1) (Color r2 g2 b2) = r1 * r2 + g1 * g2 + b1 * b2
	sub (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 - r2) (g1 - g2) (b1 - b2)
	neg (Color r g b) = Color (-r) (-g) (-b)
	normalize c @ (Color r g b) = let m = magnitude c in Color (r / m) (g / m) (b / m)
	squareMagnitude (Color r g b) = r * r + g * g + b * b

instance ColorOps Color where
	blend a c1 c2 = (c1 `scale` a) `add` (c2 `scale` (1 - a))

instance VectorOps Vector where
	scale (Vector x y z)  n = Vector (x * n) (y * n) (z * n)
	add (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)
	dot (Vector x1 y1 z1) (Vector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
	sub (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 - x2) (y1 - y2) (z1 - z2)
	neg (Vector x y z) = Vector (-x) (-y) (-z)
	normalize v @ (Vector x y z) = let m = magnitude v in Vector (x / m) (y / m) (z / m)
	squareMagnitude (Vector x y z) = x * x + y * y + z * z

roots :: (Floating a, Ord a) => a -> a -> a -> [ a ]
roots a b c | discriminant < 0 = [ ]
	          | discriminant > 0 = [ (-b - sqrt discriminant) / (2 * a), (-b + sqrt discriminant) / (2 * a)]
	          | otherwise = [ -b / (2 * a)]
            where discriminant = (square b) - 4 * a * c
