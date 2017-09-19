#!/bin/bash

function ccgen() { 
   file=$1;
   echo "Generating '"$file.cc"'";

   cm c++ $file.cc.lisp > generated_$file.cc && \
      clang-format generated_$file.cc -style="{ IndentWidth: 3 }" > $file.cc && \
      rm generated_$file.cc;

   if [ $? != 0 ]; then 
      echo "Error generating '"$file.cc"'";
      exit 1;
   fi
}

ccgen file
ccgen lexer
echo "Compiling"
g++ -o main main.cc
