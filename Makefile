all: compiler

compiler:
	haskell_stack install --local-bin-path=$(shell pwd) || stack install --local-bin-path=$(shell pwd) || /home/students/inf/PUBLIC/MRJP/Stack/stack install --local-bin-path=$(shell pwd)

clean:
	haskell_stack clean || stack clean || /home/students/inf/PUBLIC/MRJP/Stack/stack clean
	rm insc_llvm -f
	rm insc_jvm -f
