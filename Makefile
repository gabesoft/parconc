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
	stack exec fwdense1 -- 500 +RTS -s -N4

rotate-image: build
	$(ROTATE_IMG) 4 $$infile $$outfile +RTS -s -N4

fork: build
	stack exec fork

reminders: build
	stack exec reminders

logger: build
	stack exec logger

geturlscancel: build
	stack exec geturlscancel

simpleserver: build
	stack exec simpleserver -- +RTS -N4

server2: build
	stack exec server2 -- +RTS -N4

chat: build
	stack exec chat -- +RTS -N4

findseq: build
	stack exec findseq -- nonexistent .. +RTS -s

findpar: build
	stack exec findpar -- nonexistent .. +RTS -s -N

findpar2: build
	stack exec findpar2 -- 8 nonexistent .. +RTS -s -N8

findpar3: build
	stack exec findpar3 -- nonexistent .. +RTS -s -N

findpar3-log: build
	stack exec findpar3 -- nonexistent .. +RTS -s -N -l

findpar4: build
	stack exec findpar4 -- nonexistent .. +RTS -s -N