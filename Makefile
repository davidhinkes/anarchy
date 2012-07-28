all: configure build

configure:
	cabal configure --enable-tests

build:
	cabal build

test:
	cabal test

run: all
	dist/build/anarchy/anarchy
