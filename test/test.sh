#!/bin/bash

function test_dir() {
   dir=$(echo $1 | tr -d './');
   printf "%16s  " $dir
   for ccfile in $(find $dir -name "*.cc" | sort); do
      ../minicc --test-${dir} $ccfile 2>> ${dir}-err
   done
   echo
}

for dir in $(find -mindepth 1 -maxdepth 1 -type d); do
   test_dir $dir
done
for dir in $(find -mindepth 1 -maxdepth 1 -type d); do
   if [ -f ${dir}-err ]; then
      echo
      cat ${dir}-err
      rm ${dir}-err
   fi
done

