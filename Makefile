unpack-examples:
	cabal unpack parconc-examples

build:
	stack build

rpar:
	.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/rpar/rpar 1 +RTS -N2