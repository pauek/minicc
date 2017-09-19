#!/bin/bash

function ccgen() { 
   file=$1;
   echo "generating "$file.cc;
   cm c++ $file.cc.lisp > generated_$file.cc && \
      clang-format generated_$file.cc -style="{ IndentWidth: 3 }" > $file.cc && \
      rm generated_$file.cc;
}

ccgen file && \
   ccgen lexer && \
   g++ -o main main.cc;
