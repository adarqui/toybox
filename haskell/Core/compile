#!/bin/bash -x

# -dsuppress-module-prefixes -dsuppress-coercions
cc() {
 ghc --make -c -O2 -Wall -rtsopts -ddump-simpl -dsuppress-idinfo -dsuppress-type-applications -dsuppress-uniques -ddump-parsed -ddump-tc -ddump-types -ddump-simpl-phases -dverbose-core2core -dcore-lint -dshow-passes -ddump-stg -fext-core -fforce-recomp -Lsrc/Core -isrc -isrc/Core src/Core/$1.hs > src/Core/$1.out 2>&1
}

cc Int
cc IntList
cc IntConst
cc Fn1
cc Id
cc Flip
cc Compose
cc Square

cc ClassFoo
cc Foo

cc Main
