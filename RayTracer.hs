import Control.Monad
import Graphics.UI.GLUT
import MathUtils
import Scene
import Tracing

-- Divides [a] into [[a], [a], ...] with each sublist of length n,
-- except the last sublist which has length <= n.
chunkify n [ ] = [ ]
chunkify n xs =
	xs' : chunkify n rest
	where (xs', rest) = splitAt n xs

drawVert scene v @ (Vertex2 x y) = do
	let (MathUtils.Color _ r g b) = trace scene x y
	color $ Color4 r g b 1
	vertex v

display' scene chunks = do
	mapM_ renderVertex chunks
	displayCallback $= (display scene)
	where renderVertex vs = do
		renderPrimitive Points $ mapM_ (drawVert $ scene) vs
		flush

pix2vert (Size w h) (x, y) = 
	Vertex2 ((2 // w * fromIntegral x) - 1.0) ((2 // h * fromIntegral y) - 1.0)
	where x // y = fromIntegral x / fromIntegral y

vertices = do
	size <- get windowSize
	let (Size w h) = size
	return $ [ pix2vert size (x, y) | x <- [0.. w - 1], y <- [0..h - 1] ]

display scene = do
	clear [ ColorBuffer ]
	displayCallback $= (vertices >>= (display' scene) . chunkify 256)
	postRedisplay Nothing

main = do
	getArgsAndInitialize
	initialDisplayMode $= [ SingleBuffered, RGBMode ]
	initialWindowSize $= Size 1200 1024
	initialWindowPosition $= Position 100 100
	createWindow "Ray Tracer"
	clearColor $= Color4 0 0 0 0
	matrixMode $= Projection
	loadIdentity
	ortho (-1) 1 (-1) 1 (-1) 1
	displayCallback $= (display $ scene)
	mainLoop
