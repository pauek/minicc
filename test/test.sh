#!/bin/bash

colsize=60

verbose="false"
if [ ! -z $1 ] && [ $1 = "-v" ]; then
   verbose="true"
   shift
fi

function test_dir() {
   dir=$(echo $1 | tr -d './');
   if [ $verbose == "true" ]; then
      echo $dir
      echo "--------------"
   else
      printf "%11s  " $dir
   fi
   if [ $verbose = "true" ]; then echo; fi
   for ccfile in $(find $dir -name "*.cc" | sort | xargs -n $colsize | sed '2,$s/^/<endl> /'); do
      if [ $ccfile = "<endl>" ]; then
         if [ $verbose == "false" ]; then
            printf "\n%11s  " ""
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

make -C ..

DIRS=$*
if [ -z "$DIRS" ]; then
    DIRS=$(find -mindepth 1 -maxdepth 1 -type d | sort)
fi
for dir in $DIRS; do
   test_dir $dir
done
for dir in $DIRS; do
   if [ -f ${dir}-err ]; then
      cat ${dir}-err > /dev/stderr
      rm ${dir}-err
   fi
done

