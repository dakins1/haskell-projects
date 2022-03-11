# Commands:

.PHONY: build init test clean doc deploy stage

build:
	ghc --make -O -o fungi Main.hs

prof:
	ghc --make -prof -o fungi Main.hs

all: build test

# Cleaning commands:
clean:
	rm -f fungi
	rm -f *.hi
	rm -f *.o

setup:
	cabal install ansi-terminal
