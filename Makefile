all: test

build:
	mkdir -p bin obj
	ghc --make -Wall -o bin/RayTracer -outputdir obj *.hs

clean:
	rm -r bin obj

test: build
	bin/RayTracer
