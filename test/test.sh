#!/bin/bash

function test_dir() {
   dir=$(echo $1 | tr -d './');
   printf "%10s  " $dir
   for ccfile in $(find $dir -name "*.cc" | sort); do
      ../minicc --test-${dir} $ccfile 2>> ${dir}-err
      code=$?
      if [ $code -ne 0 ]; then
         echo "[error code $code in $ccfile]" >> ${dir}-err
         echo -n "E"
      fi
   done
   echo
}

for dir in $(find -mindepth 1 -maxdepth 1 -type d | sort); do
   test_dir $dir
done
for dir in $(find -mindepth 1 -maxdepth 1 -type d); do
   if [ -f ${dir}-err ]; then
      cat ${dir}-err > /dev/stderr
      rm ${dir}-err
   fi
done

