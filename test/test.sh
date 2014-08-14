#!/bin/bash

colsize=60

verbose="false"
if [ ! -z $1 ] && [ $1 = "-v" ]; then
   verbose="true"
fi

function test_dir() {
   dir=$(echo $1 | tr -d './');
   if [ $verbose == "true" ]; then
      echo $dir
      echo "--------------"
   else
      printf "%10s  " $dir
   fi
   if [ $verbose = "true" ]; then echo; fi
   for ccfile in $(find $dir -name "*.cc" | sort | xargs -n $colsize | sed 's/$/ <endl>/'); do
      if [ $ccfile = "<endl>" ]; then
         if [ $verbose == "false" ]; then
            printf "\n%10s  " ""
         fi
      else
         if [ $verbose = "true" ]; then
            echo -n $ccfile" "
         fi
         ../minicc --test-${dir} $ccfile 2>> ${dir}-err
         code=$?
         if [ $code -ne 0 ]; then
            echo "[error code $code in $ccfile]" >> ${dir}-err
            echo -n "E"
         fi
         if [ $verbose = "true" ]; then echo; fi
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

