#!/bin/bash

function test() {
   ../minicc $1 > outA 2> errA
   awk '/\/* out/, /*\// { print }' include_local_global.cc | tail -n+2 | head -n-1 > outB
   awk '/\/* err/, /*\// { print }' include_local_global.cc | tail -n+2 | head -n-1 > errB
   diff outA outB
   diff errA errB
   rm -f out[AB] err[AB]
}

test include_local_global.cc

# for codefile in *.cc; do
#    test $codefile
#done
