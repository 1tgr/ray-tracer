all: build

build:
	mkdir -p bin obj
	ghc --make -Wall -o bin/ray-tracer -outputdir obj *.hs

clean:
	rm -r bin obj

test: build
	bin/ray-tracer
