import Control.Monad
import Control.Parallel.Strategies
import Graphics.UI.GLUT as GLUT
import MathUtils
import Scene
import Shapes
import Tracing

drawPixel :: (ColorComponent num, Floating num, VertexComponent a) => (Vertex2 a, Maybe (Hit vector (NumColor num) num)) -> IO ()
drawPixel (v, colour) = do
  let NumColor r g b = snd $ unpackTraceResult colour
  GLUT.color $ Color4 r g b 1
  vertex v

drawChunk :: (ColorComponent num, Floating num, VertexComponent a) => (a -> a -> Maybe (Hit vector (NumColor num) num)) -> [ Vertex2 a ] -> IO ()
drawChunk traceFn chunk = do
  let pixels = (parMap rwhnf) (\v @ (Vertex2 x y) -> (v, traceFn x y)) chunk
  renderPrimitive Points $ mapM_ drawPixel pixels
  flush

chunkify :: Int -> [ t ] -> [ [ t ] ]
chunkify _ [ ] = [ ]
chunkify n xs = let (xs', rest) = splitAt n xs in xs' : chunkify n rest

(//) :: (Integral a, Integral b, Fractional c) => a -> b -> c
x // y = fromIntegral x / fromIntegral y

display' :: (ColorComponent num, Floating num, Fractional a, VertexComponent a) => (a -> a -> Maybe (Hit vector (NumColor num) num)) -> Size -> DisplayCallback
display' traceFn size = do
  mapM_ (drawChunk traceFn) chunks
  displayCallback $= (display traceFn)
  where (Size w h) = size
        ratio = w // h
        pix2vert (x, y) = Vertex2 (((2 * ratio) / fromIntegral w * fromIntegral x) - ratio) (((2::Integer) // h * fromIntegral y) - 1)
        chunks = chunkify 256 [ pix2vert (x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1] ]

display :: (ColorComponent num, Floating num, Fractional a, VertexComponent a) => (a -> a -> Maybe (Hit vector (NumColor num) num)) -> DisplayCallback
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

main :: IO ()
main = do
  getArgsAndInitialize
  initialDisplayMode $= [ SingleBuffered, RGBMode ]
  initialWindowSize $= Size 1024 1024
  initialWindowPosition $= Position 100 100
  createWindow "Ray Tracer"
  clearColor $= Color4 0 0 0 0
  displayCallback $= (display $ trace (scene :: [ Shape (NumVector Float) (NumColor Float) Float ]))
  mainLoop