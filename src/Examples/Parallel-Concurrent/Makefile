all:
	ghc -rtsopts -threaded -Wall Fib.hs -prof -O2

run:
	./Fib 38 5300 +RTS -N1
	./Fib 38 5300 +RTS -N2
