all: compiler

compiler:
	haskell_stack install --local-bin-path=$(shell pwd) || stack install --local-bin-path=$(shell pwd)
