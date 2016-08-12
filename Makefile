unpack-examples:
	cabal unpack parconc-examples

build:
	stack build

install-threadscope:
	stack install gtk
	stack install threadscope

rpar: build
	.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/rpar/rpar 1 +RTS -N2

sudoku1: build
	.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/sudoku1/sudoku1 ./data/sudoku17.1000.txt +RTS -s

sudoku2: build
	.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/sudoku2/sudoku2 ./data/sudoku17.1000.txt +RTS -N2 -s

sudoku2-log: build
	.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/sudoku2/sudoku2 ./data/sudoku17.1000.txt +RTS -N2 -l

sudoku3: build
	.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/sudoku3/sudoku3 ./data/sudoku17.1000.txt +RTS -N4 -s

sudoku3-log: build
	.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/sudoku3/sudoku3 ./data/sudoku17.1000.txt +RTS -N4 -l

sudoku3-threadscope:
	threadscope sudoku3.eventlog