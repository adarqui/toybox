# haskell experimentation

## extremely useful language extensions

OverloadedStrings
MonadComprehensions

## consider

filename/path manipulation:
- filepath

binary manipulation:
- binary
- binary-strict

configuration files:
- configurator

api:
- servant

regex:
- regex-posix

system types:
- System.Posix.Types
- https://hackage.haskell.org/package/base-4.8.0.0/docs/System-Posix-Types.html#t:UserID

users / groups:
- unix: System.Posix

parsing haskell:
- haskell-src-meta


## code analysis

SourceGraph
graphmod

```
find Data -name '*.hs' | xargs graphmod -q | xdot -
find src -name '*.hs' | xargs graphmod -q -p --no-cluster | xdot -
```

lscabal

## study

haskell-suite/haskell-src-exts/src/Language/Haskell/Exts/Annotated/Syntax.hs
Gabriel439/Haskell-Pipes-Library
ekmett/free/examples
ekmett/keys
ekmett/algebra
jwiegley/coq-pipes
mstksg/inCode
mstksg/auto
mstksg/auto-examples
yav/mini-sat/tree
yav/simple-smt
yav/mascarade
TomMD/network-data
GaloisInc/e2eviv - end-to-end verifiable internet voting

## reverse dependencies

http://packdeps.haskellers.com/reverse
