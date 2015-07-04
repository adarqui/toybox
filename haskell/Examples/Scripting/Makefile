all:
	echo "module Script where" > gen/Script.hs
	cabal install
	./.cabal-sandbox/bin/propfind props/Props.hs
	cabal repl
