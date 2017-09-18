#!/bin/bash
cm c++ lexer.cc.lisp > _lexer.cc && \
   clang-format _lexer.cc -style="{ IndentWidth: 3 }" > lexer.cc && \
   rm -f _lexer.cc && \
   g++ -g3 -o lexer lexer.cc
