all:
	cabal sandbox init
	cabal install -j

test_init:
	cabal install --enable-test --only-dependencies
	cabal configure --enable-tests
	cabal build

docs:
	cabal haddock --hyperlink-source
