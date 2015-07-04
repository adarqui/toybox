all: init test_init
	cabal sandbox init
	cabal install -j

init:
	cabal sandbox init
	cabal sandbox add-source ../stm

test_init:
	cabal sandbox init
	cabal install --enable-test --only-dependencies
	cabal configure --enable-tests
	cabal build

docs:
	cabal haddock --hyperlink-source
