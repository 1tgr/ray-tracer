GHC = /opt/local/bin/ghc

all:
	$(GHC) -threaded --make RayTracer
	$(GHC) --make RayTracerDebug
