import Control.Monad
import Control.Parallel.Strategies
import Data.IORef
import Data.Maybe
import Debug.Trace as Trace
import Graphics.UI.GLUT as GLUT
import MathUtils
import Scene
import Shapes
import Tracing

colorFromNesting 0 = Color4 1 0 0 1
colorFromNesting 1 = Color4 0 1 0 1
colorFromNesting 2 = Color4 0 0 1 1
colorFromNesting nesting = Color4 0 0 (1 - (fromIntegral nesting) / 16.0::GLfloat) 1

line nesting start direction =
	lineTo nesting start (start `add` direction)

lineTo nesting (Vector x1 y1 z1) (Vector x2 y2 z2) = do
	GLUT.color $ colorFromNesting nesting
	vertex $ Vertex3 x1 y1 z1
	vertex $ Vertex3 x2 y2 z2

displayHit nesting lastLineFn Hit { ray = (Ray start direction), secondaryHits = secondaryHits } = do
	line nesting start direction
	lastLineFn start
	mapM_ (displayHit (nesting + 1) (lineTo nesting start)) secondaryHits

displayFirstHit hit @ Hit { ray = (Ray start direction), secondaryHits = secondaryHits } = do
	line 0 start direction
	mapM_ (displayHit 1 (lineTo 0 start)) secondaryHits

display angle hits = do
	a <- get angle
	clear [ ColorBuffer ]
	preservingMatrix $ do
		rotate a $ Vector3 (1::GLfloat) 0 0
		rotate 30 $ Vector3 0 (1::GLfloat) 0
		renderPrimitive Lines $ mapM_ displayFirstHit hits
	flush
	postRedisplay Nothing

reshape (Size w h) = do
	let ratio = fromIntegral w / fromIntegral h
	matrixMode $= Projection
	loadIdentity
	ortho (-ratio * 10) (ratio * 10) (-10) 10 (-10) 10
	matrixMode $= Modelview 0
	postRedisplay Nothing

idle angle lastTime = do
	a <- get angle
	time <- get elapsedTime
	let delta = fromIntegral (time - lastTime) / 1000
	angle $= a + 10 * delta
	idleCallback $= Just (idle angle time)
	postRedisplay Nothing

keyboardMouse :: IORef Float -> Bool -> KeyboardMouseCallback
keyboard angle True (Char ' ') = do
	idleCallback $= Nothing
	keyboardMouseCallback $= Just (keyboardMouse angle False)

keyboard angle False (Char ' ') = do
	time <- get elapsedTime
	idleCallback $= Just (idle angle time)
	keyboardMouseCallback $= Just (keyboardMouse angle True)

keyboard _ _ _ = return ()

keyboardMouse angle spinning key Down Modifiers { shift = Up, ctrl = Up, alt = Up } _ = keyboard angle spinning key
keyboardMouse _ _ _ _ _ _ = return ()

main = do
	getArgsAndInitialize
	initialDisplayMode $= [ SingleBuffered, RGBMode ]
	initialWindowSize $= Size 1024 1024
	initialWindowPosition $= Position 100 100
	createWindow "Ray Tracer"
	clearColor $= Color4 0 0 0 0
	angle <- newIORef 0.0
	let hits = catMaybes [ Tracing.trace scene (x / 5) (y / 5) | x <- [-5 .. 5], y <- [-5 .. 5] ]
	displayCallback $= (display angle hits)
	reshapeCallback $= Just reshape
	keyboardMouseCallback $= Just (keyboardMouse angle False)
	mainLoop
