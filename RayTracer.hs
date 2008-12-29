import Control.Monad
import Graphics.UI.GLUT
import MathUtils
import Scene
import Tracing

drawPixel traceFn v @ (Vertex2 x y) = do
	let (MathUtils.Color r g b) = traceFn x y
	color $ Color4 r g b 1
	vertex v

drawChunk traceFn chunk = do
	renderPrimitive Points $ mapM_ (drawPixel traceFn) chunk
	flush

chunkify n [ ] = [ ]
chunkify n xs =
	xs' : chunkify n rest
	where (xs', rest) = splitAt n xs

x // y = fromIntegral x / fromIntegral y

display' traceFn size = do
	mapM_ (drawChunk traceFn) chunks
	displayCallback $= (display traceFn)
	where
		(Size w h) = size
		ratio = w // h
		pix2vert (x, y) = Vertex2 (((2.0 * ratio) / fromIntegral w * fromIntegral x) - ratio) ((2 // h * fromIntegral y) - ratio)
		chunks = chunkify 256 [ pix2vert (x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1] ]

display traceFn = do
	size <- get windowSize
	let (Size w h) = size
	let ratio = w // h
	clear [ ColorBuffer ]
	matrixMode $= Projection
	loadIdentity
	ortho (-ratio) ratio (-1) 1 (-1) 1
	matrixMode $= Modelview 0
	displayCallback $= (display' traceFn size)
	postRedisplay Nothing

main = do
	getArgsAndInitialize
	initialDisplayMode $= [ SingleBuffered, RGBMode ]
	initialWindowSize $= Size 1024 1024
	initialWindowPosition $= Position 100 100
	createWindow "Ray Tracer"
	clearColor $= Color4 0 0 0 0
	displayCallback $= (display $ trace scene)
	mainLoop
