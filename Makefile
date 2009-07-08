all:
	ghc -threaded --make RayTracer
	ghc --make RayTracerDebug
