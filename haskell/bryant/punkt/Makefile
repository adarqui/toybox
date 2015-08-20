all: init test_init
	cabal install -j

init:
	cabal sandbox init
	cabal sandbox add-source ~x/code/github/hackage/regex-tdfa-text-1.0.0.2

test_init:
	cabal install --enable-test --only-dependencies
	cabal configure --enable-tests
	cabal build

docs:
	cabal haddock --hyperlink-source
