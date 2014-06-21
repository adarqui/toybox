export FIB=42
export EULER=5300


p() {
 printf "\n\n------------------------- $1:\n\n"
}

p "SumFibEuler $FIB $EULER"
./.cabal-sandbox/bin/SumFibEuler $FIB $EULER +RTS -s -P
cat SumFibEuler.prof
rm -f SumFibEuler.prof

p "SumFibEuler $FIB $EULER +RTS -N2"
./.cabal-sandbox/bin/SumFibEuler $FIB $EULER +RTS -N2 -s -P
cat SumFibEuler.prof
rm -f SumFibEuler.prof

p "ParSumFibEuler2 $FIB $EULER"
./.cabal-sandbox/bin/ParSumFibEuler2 $FIB $EULER +RTS -s -P
cat ParSumFibEuler2.prof
rm -f ParSumFibEuler2.prof

p "ParSumFibEuler2 $FIB $EULER +RTS -N2"
./.cabal-sandbox/bin/ParSumFibEuler2 $FIB $EULER +RTS -N2 -s -P
cat ParSumFibEuler2.prof
rm -f ParSumFibEuler2.prof

p "ParSumFibEuler4 $FIB $EULER"
./.cabal-sandbox/bin/ParSumFibEuler4 $FIB $EULER +RTS -s -P
cat ParSumFibEuler4.prof
rm -f ParSumFibEuler4.prof

p "ParSumFibEuler4 $FIB $EULER +RTS -N2"
./.cabal-sandbox/bin/ParSumFibEuler4 $FIB $EULER +RTS -N2 -s -P
cat ParSumFibEuler4.prof
rm -f ParSumFibEuler4.prof
