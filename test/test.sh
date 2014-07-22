#!/bin/bash

colsize=60

function test_dir() {
   dir=$(echo $1 | tr -d './');
   printf "%10s  " $dir
   for ccfile in $(find $dir -name "*.cc" | sort | xargs -n $colsize | sed 's/$/ <endl>/'); do
      if [ $ccfile = "<endl>" ]; then
         printf "\n%10s  " ""
      else
         ../minicc --test-${dir} $ccfile 2>> ${dir}-err
         code=$?
         if [ $code -ne 0 ]; then
            echo "[error code $code in $ccfile]" >> ${dir}-err
            echo -n "E"
         fi
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

