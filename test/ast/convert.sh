#!/bin/bash

for ccfile in ../parser/*.cc; do 
   out=$(basename $ccfile)
   ../code.sh $ccfile >> $out
   echo "[[out]]------------------------------------" >> $out
   ../code.sh $ccfile | ../../minicc --ast >> $out
   echo "[[err]]------------------------------------" >> $out
done
   