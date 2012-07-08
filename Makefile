all: configure build
	cabal build
configure:
	cabal configure
build:
	cabal build
run: all	
	dist/build/anarchy/anarchy
