module MathUtils where

data Vector = Vector Float Float Float 
type Position = Vector
type Direction = Vector
type Normal = Vector
data Color = Color Float Float Float Float

magnitude v = sqrt $ squareMagnitude v
square n = n * n

class VectorOps a where
	scale :: a -> Float -> a
	add :: a -> a -> a
	dot :: a -> a -> Float
	sub :: a -> a -> a
	neg :: a -> a
	normalize :: a -> a
	squareMagnitude :: a -> Float

class (VectorOps a) => ColorOps a where
	blend :: a -> a -> a

instance VectorOps Color where
	scale (Color a r g b) x = Color a (r * x) (g * x) (b * x)
	add (Color _ r1 g1 b1) (Color _ r2 g2 b2) = Color 1 (r1 + r2) (g1 + g2) (b1 + b2)
	dot (Color _ r1 g1 b1) (Color _ r2 g2 b2) = r1 * r2 + g1 * g2 + b1 * b2
	sub (Color _ r1 g1 b1) (Color _ r2 g2 b2) = Color 1 (r1 - r2) (g1 - g2) (b1 - b2)
	neg (Color _ r g b) = Color 1 (-r) (-g) (-b)
	normalize c =
		Color a (r / m) (g / m) (b / m)
		where
			(Color a r g b) = c
			m = magnitude c
	squareMagnitude (Color _ r g b) = r * r + g * g + b * b

instance ColorOps Color where
	blend (Color _ r g b) (Color 0 _ _ _) = Color 1 r g b
	blend (Color a r1 g1 b1) (Color _ r2 g2 b2) = Color 1 (r1 * a + r2 * (1 - a)) (g1 * a + g2 * (1 - a)) (b1 * a + b2 * (1 - a))

instance VectorOps Vector where
	scale (Vector x y z)  n = Vector (x * n) (y * n) (z * n)
	add (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)
	dot (Vector x1 y1 z1) (Vector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
	sub (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 - x2) (y1 - y2) (z1 - z2)
	neg (Vector x y z) = Vector (-x) (-y) (-z)
	normalize v @ (Vector x y z) =
		Vector (x / m) (y / m) (z / m)
		where m = magnitude v
	squareMagnitude (Vector x y z) = x * x + y * y + z * z

roots a b c =
    if discriminant >= 0
	then [ (-b - sqrt discriminant) / (2 * a), (-b + sqrt discriminant) / (2 * a)]
	else [ ]
	where discriminant = (square b) - 4 * a * c
