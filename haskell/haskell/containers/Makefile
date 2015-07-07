all: init test_init
	cabal install -j

init:
	cabal sandbox init
	cabal sandbox add-source ../../bos/attoparsec

test_init:
	cabal install --enable-test --only-dependencies
	cabal configure --enable-tests
	cabal build

docs:
	cabal haddock --hyperlink-source
