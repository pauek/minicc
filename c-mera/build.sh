#!/bin/bash
cm c++ lexer.c.lisp > _lexer.cc
clang-format _lexer.cc -style="{ IndentWidth: 3 }" > lexer.cc
g++ -o lexer lexer.cc
rm -f _lexer.cc

