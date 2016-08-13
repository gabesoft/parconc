PARMONAD = $(shell stack exec which parmonad)
RPAR = $(shell stack exec which rpar)
SUDOKU1 = $(shell stack exec which sudoku1)
SUDOKU2 = $(shell stack exec which sudoku2)
SUDOKU3 = $(shell stack exec which sudoku3)
FWDENSE = $(shell stack exec which fwdense)
FWDENSE1 = $(shell stack exec which fwdense1)
ROTATE_IMG = $(shell stack exec which rotateimage)
DATA = ./data

unpack-examples:
	cabal unpack parconc-examples

build:
	stack build

install-threadscope: export LDFLAGS=-L/usr/local/opt/icu4c/lib
install-threadscope: export CPPFLAGS=-I/usr/local/opt/icu4c/include
install-threadscope:
	stack install gtk
	stack install threadscope

rpar: build
	$(RPAR) 1 +RTS -N2

sudoku1: build
	$(SUDOKU1) $(DATA)/sudoku17.1000.txt +RTS -s

sudoku2: build
	$(SUDOKU2) $(DATA)/sudoku17.1000.txt +RTS -N2 -s

sudoku2-log: build
	$(SUDOKU2) $(DATA)/sudoku17.1000.txt +RTS -N2 -l

sudoku3: build
	$(SUDOKU3) $(DATA)/sudoku17.1000.txt +RTS -N4 -s

sudoku3-log: build
	$(SUDOKU3) $(DATA)/sudoku17.1000.txt +RTS -N4 -l

sudoku3-threadscope:
	threadscope sudoku3.eventlog

parmonad: build
	$(PARMONAD) 34 35 +RTS -N2

fwdense: build
	$(FWDENSE) 500 +RTS -s

fwdense1: build
	$(FWDENSE1) 500 +RTS -s -N4

rotate-image: build
	$(ROTATE_IMG) 4 $$infile $$outfile +RTS -s -N4