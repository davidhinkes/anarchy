all: configure build

configure:
	cabal configure --enable-tests

build:
	cabal build

test: all
	cabal test

run: all
	dist/build/anarchy/anarchy
